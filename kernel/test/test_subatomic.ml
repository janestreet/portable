open! Core
open! Portable_kernel
open Expect_test_helpers_core
open Subatomic

[%%template
module
  [@synchro.explicit name = (unsync, sync), impl = (unsync, sync)]
  [@mode.explicit maybe_contended = (uncontended, contended)] Test_one (Impl : sig
  @@ portable
    type ('a : value_or_null) t

    val make_example : 'a -> 'a t

    include
      Subatomic.Private.S
      [@synchro.explicit name impl] [@mode.explicit maybe_contended]
      with type ('a : value_or_null) t := 'a t
  end) : sig
  @@ portable
  include
    Subatomic.Private.S
    [@synchro.explicit name impl] [@mode.explicit maybe_contended]
    with type ('a : value_or_null) t := 'a Impl.t
end = struct
  open Impl

  let get = (get [@synchro name])
  let set = (set [@synchro name])

  let%expect_test "get and set" =
    let t1 = Impl.make_example 1 in
    let get_before_set = get t1 in
    set t1 2;
    let get_after_set = get t1 in
    (* [get] is the only operation that works on a shared subatomic with contents that
       don't cross contention. *)
    let t2 = Impl.make_example (ref "a") in
    let get_untouched = (get t2).contents in
    print_s
      [%message (get_before_set : int) (get_after_set : int) (get_untouched : string)];
    [%expect
      {|
      ((get_before_set 1)
       (get_after_set  2)
       (get_untouched  a))
      |}];
    (* no allocation *)
    require_no_allocation (fun () -> set t1 (get t1 + 1));
    [%expect {| |}]
  ;;

  let exchange = (exchange [@synchro name])

  let%expect_test "exchange" =
    let t = Impl.make_example 1 in
    let result = exchange t 2 in
    let new_value = get t in
    print_s [%message (result : int) (new_value : int)];
    [%expect
      {|
      ((result    1)
       (new_value 2))
      |}];
    (* no allocation *)
    require_no_allocation (fun () -> ignore (exchange t 3 : int));
    [%expect {| |}]
  ;;

  let compare_and_set = (compare_and_set [@synchro name])

  let%expect_test "compare_and_set" =
    let t = Impl.make_example 1 in
    let compare_failed = compare_and_set t ~if_phys_equal_to:4 ~replace_with:10 in
    let current_value = get t in
    print_s
      [%message
        (compare_failed : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
    [%expect
      {|
      ((compare_failed Compare_failed)
       (current_value  1))
      |}];
    let set_here = compare_and_set t ~if_phys_equal_to:1 ~replace_with:10 in
    let current_value = get t in
    print_s
      [%message (set_here : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
    [%expect
      {|
      ((set_here      Set_here)
       (current_value 10))
      |}];
    (* no allocation *)
    require_no_allocation (fun () ->
      assert (
        Poly.equal (compare_and_set t ~if_phys_equal_to:10 ~replace_with:11) Set_here);
      assert (
        Poly.equal
          (compare_and_set t ~if_phys_equal_to:10000 ~replace_with:12)
          Compare_failed));
    [%expect {| |}]
  ;;

  let compare_exchange = (compare_exchange [@synchro name])

  let%expect_test "compare_exchange" =
    let t = Impl.make_example 1 in
    let result = compare_exchange t ~if_phys_equal_to:4 ~replace_with:10 in
    let current_value = get t in
    print_s [%message (result : int) (current_value : int)];
    [%expect
      {|
      ((result        1)
       (current_value 1))
      |}];
    let result = compare_exchange t ~if_phys_equal_to:1 ~replace_with:10 in
    let current_value = get t in
    print_s [%message (result : int) (current_value : int)];
    [%expect
      {|
      ((result        1)
       (current_value 10))
      |}];
    [%expect {| |}];
    (* no allocation *)
    require_no_allocation (fun () ->
      assert (compare_exchange t ~if_phys_equal_to:10 ~replace_with:11 = 10);
      assert (compare_exchange t ~if_phys_equal_to:10 ~replace_with:12 = 11));
    [%expect {| |}]
  ;;

  let update = (update [@synchro name])
  let update_and_return = (update_and_return [@synchro name])

  let%expect_test "update and update_and_return" =
    let t = Impl.make_example 1 in
    let result = update_and_return t ~pure_f:(fun x -> x + 1) in
    let new_value = get t in
    print_s [%message (result : int) (new_value : int)];
    [%expect
      {|
      ((result    1)
       (new_value 2))
      |}];
    (* no allocation *)
    require_no_allocation (fun () ->
      update t ~pure_f:(fun x -> x + 1);
      ignore (update_and_return t ~pure_f:(fun x -> x + 1) : int));
    [%expect {| |}]
  ;;

  let incr = (incr [@synchro name])
  let decr = (decr [@synchro name])
  let fetch_and_add = (fetch_and_add [@synchro name])
  let add = (add [@synchro name])
  let sub = (sub [@synchro name])
  let logand = (logand [@synchro name])
  let logor = (logor [@synchro name])
  let logxor = (logxor [@synchro name])

  let%expect_test "int ops" =
    let t = Impl.make_example 0 in
    let message_queue = Queue.create () in
    let print sexp = Queue.enqueue message_queue sexp in
    print [%message (get t : int)];
    print [%message (incr t : unit)];
    print [%message (get t : int)];
    print [%message (decr t : unit)];
    print [%message (get t : int)];
    print [%message (fetch_and_add t 2 : int)];
    print [%message (get t : int)];
    print [%message (add t 3 : unit)];
    print [%message (get t : int)];
    print [%message (sub t 2 : unit)];
    print [%message (get t : int)];
    print [%message (logand t 5 : unit)];
    print [%message (get t : int)];
    print [%message (logor t 6 : unit)];
    print [%message (get t : int)];
    print [%message (logxor t 5 : unit)];
    print [%message (get t : int)];
    print_s [%sexp (message_queue : Sexp.t Queue.t)];
    [%expect
      {|
      (("get t" 0)
       ("incr t" ())
       ("get t" 1)
       ("decr t" ())
       ("get t"             0)
       ("fetch_and_add t 2" 0)
       ("get t"             2)
       ("add t 3" ())
       ("get t" 5)
       ("sub t 2" ())
       ("get t" 3)
       ("logand t 5" ())
       ("get t" 1)
       ("logor t 6" ())
       ("get t" 7)
       ("logxor t 5" ())
       ("get t" 2))
      |}];
    (* no allocation *)
    require_no_allocation (fun () ->
      incr t;
      decr t;
      ignore (fetch_and_add t 1 : int);
      add t 3;
      sub t 4;
      logand t 5;
      logor t 6;
      logxor t 7);
    [%expect {| |}]
  ;;

  (* Re-template names *)
  [@@@synchro.default name]

  let get = get
  let set = set
  let exchange = exchange
  let compare_and_set = compare_and_set
  let compare_exchange = compare_exchange
  let update = update
  let update_and_return = update_and_return
  let fetch_and_add = fetch_and_add
  let add = add
  let sub = sub
  let logand = logand
  let logor = logor
  let logxor = logxor
  let incr = incr
  let decr = decr
end

module%test Subatomic : module type of Subatomic = struct
  type nonrec ('a : value_or_null) t = 'a t

  module Int_t = struct
    type nonrec t = int t [@@deriving equal, sexp]
  end

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let equal = equal

  let%expect_test "sexp serialization" =
    print_and_check_sexpable (module Int_t) [ make 1; make 2; make 3 ];
    [%expect
      {|
      1
      2
      3
      |}]
  ;;

  external make
    : ('a : value_or_null).
    'a -> ('a t[@local_opt])
    @@ portable
    = "%makemutable"

  let make_alone = make_alone

  let%expect_test "make" =
    (* make works with multiple kinds *)
    ignore (make 1 : int t);
    ignore (make Null : int Or_null.t t);
    ignore (make (ref 1) : int ref t);
    ignore (make (make 1) : int t t);
    (* make_alone works with multiple kinds *)
    ignore (make_alone 1 : int t);
    ignore (make_alone Null : int Or_null.t t);
    ignore (make_alone (ref 1) : int ref t);
    ignore (make_alone (make 1) : int t t);
    [%expect {| |}]
  ;;

  include Test_one [@synchro.explicit unsync unsync] [@mode.explicit uncontended] (struct
      include Subatomic

      let make_example = make
    end)

  include Test_one [@synchro.explicit sync sync] [@mode.explicit contended] (struct
      include Subatomic

      let make_example = make
    end)

  module Shared = struct
    include Test_one [@synchro.explicit unsync sync] [@mode.explicit contended] (struct
        type ('a : value_or_null) t = 'a Subatomic.t

        include Subatomic.Shared

        let make_example = make
      end)
  end

  module Loc = struct
    open Loc

    type nonrec ('a : value_or_null) t = 'a t

    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
      @@ portable
      = "%identity"

    type 'a example = { mutable field : 'a [@atomic] }

    let make_example value =
      let example = { field = value } in
      unsafe_of_atomic_loc [%atomic.loc example.field]
    ;;

    let sexp_of_t = sexp_of_t

    let%expect_test "sexp_of_t" =
      let t = make_example 1 in
      print_s [%sexp (t : int t)];
      [%expect {| 1 |}]
    ;;

    include
      Test_one [@synchro.explicit unsync unsync] [@mode.explicit uncontended] (struct
        include Subatomic.Loc

        let make_example = make_example
      end)

    include Test_one [@synchro.explicit sync sync] [@mode.explicit contended] (struct
        include Subatomic.Loc

        let make_example = make_example
      end)

    module Shared = struct
      external unsafe_of_atomic_loc
        : ('a : value_or_null).
        ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
        @@ portable
        = "%identity"

      include Test_one [@synchro.explicit unsync sync] [@mode.explicit contended] (struct
          type ('a : value_or_null) t = 'a Subatomic.Loc.t

          include Subatomic.Loc.Shared

          let make_example = make_example
        end)
    end
  end

  module Private = Subatomic.Private
end]

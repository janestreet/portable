open! Core
open! Portable_kernel
open Expect_test_helpers_core
open Subatomic

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

  let get = get
  let set = set

  let%expect_test "get and set" =
    let r1 = ref 1 in
    let atomic = make r1 in
    require (phys_equal (get atomic) r1);
    let r2 = ref 2 in
    set atomic r2;
    require (phys_equal (get atomic) r2);
    [%expect {| |}]
  ;;

  module Shared = struct
    open Shared

    let get = get
    let set = set

    let%expect_test "get of ref" =
      let r = ref 1 in
      let atomic @ shared = make r in
      require (phys_equal (get atomic) r);
      [%expect {| |}]
    ;;

    let%expect_test "get and set" =
      let atomic @ shared = make 1 in
      require (phys_equal (get atomic) 1);
      set atomic 2;
      require (phys_equal (get atomic) 2);
      [%expect {| |}]
    ;;

    let exchange = exchange

    let%expect_test "exchange" =
      let atomic @ shared = make 1 in
      let result = exchange atomic 2 in
      let new_value = get atomic in
      print_s [%message (result : int) (new_value : int)];
      [%expect
        {|
        ((result    1)
         (new_value 2))
        |}]
    ;;

    let compare_and_set = compare_and_set

    let%expect_test "compare_and_set" =
      let atomic = make 1 in
      let compare_failed = compare_and_set atomic ~if_phys_equal_to:4 ~replace_with:10 in
      let current_value = get atomic in
      print_s
        [%message
          (compare_failed : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
      [%expect
        {|
        ((compare_failed Compare_failed)
         (current_value  1))
        |}];
      let set_here = compare_and_set atomic ~if_phys_equal_to:1 ~replace_with:10 in
      let current_value = get atomic in
      print_s
        [%message (set_here : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
      [%expect
        {|
        ((set_here      Set_here)
         (current_value 10))
        |}]
    ;;

    let compare_exchange = compare_exchange

    let%expect_test "compare_exchange" =
      let atomic = make 1 in
      let result = compare_exchange atomic ~if_phys_equal_to:4 ~replace_with:10 in
      let current_value = get atomic in
      print_s [%message (result : int) (current_value : int)];
      [%expect
        {|
        ((result        1)
         (current_value 1))
        |}];
      let result = compare_exchange atomic ~if_phys_equal_to:1 ~replace_with:10 in
      let current_value = get atomic in
      print_s [%message (result : int) (current_value : int)];
      [%expect
        {|
        ((result        1)
         (current_value 10))
        |}]
    ;;

    let update = update
    let update_and_return = update_and_return

    let%expect_test "update and update_and_return" =
      let atomic = make 1 in
      let result = update_and_return atomic ~pure_f:(fun x -> x + 1) in
      let new_value = get atomic in
      print_s [%message (result : int) (new_value : int)];
      [%expect
        {|
        ((result    1)
         (new_value 2))
        |}]
    ;;

    let%expect_test "update doesn't allocate" =
      let atomic = make 1 in
      require_no_allocation (fun () -> update atomic ~pure_f:(fun x -> x + 1));
      [%expect {| |}]
    ;;

    let incr = incr
    let decr = decr
    let fetch_and_add = fetch_and_add
    let add = add
    let sub = sub
    let logand = logand
    let logor = logor
    let logxor = logxor

    let%expect_test "int ops" =
      let atomic = make 0 in
      incr atomic;
      require_equal (module Int) (get atomic) 1;
      decr atomic;
      require_equal (module Int) (get atomic) 0;
      require_equal (module Int) (fetch_and_add atomic 2) 0;
      require_equal (module Int) (get atomic) 2;
      add atomic 3;
      require_equal (module Int) (get atomic) 5;
      sub atomic 2;
      require_equal (module Int) (get atomic) 3;
      logand atomic 5;
      require_equal (module Int) (get atomic) 1;
      logor atomic 6;
      require_equal (module Int) (get atomic) 7;
      logxor atomic 5;
      require_equal (module Int) (get atomic) 2;
      [%expect {| |}]
    ;;
  end

  module Loc = struct
    open Loc

    type nonrec ('a : value_or_null) t = 'a t

    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
      @@ portable
      = "%identity"

    module Ex = struct
      type t =
        { mutable int_field : int [@atomic]
        ; mutable string_ref_field : string ref [@atomic]
        }

      let mk x y = { int_field = x; string_ref_field = ref y }
      let int_loc t = unsafe_of_atomic_loc [%atomic.loc t.int_field]
      let string_ref_loc t = unsafe_of_atomic_loc [%atomic.loc t.string_ref_field]
    end

    let sexp_of_t = sexp_of_t

    let%expect_test "sexp_of_t" =
      let loc = Ex.mk 1 "a" |> Ex.int_loc in
      print_s [%sexp (loc : int t)];
      [%expect {| 1 |}]
    ;;

    let get = get
    let set = set

    let%expect_test "get and set" =
      let string_ref_loc = Ex.mk 1 "a" |> Ex.string_ref_loc in
      require_equal (module String) !(get string_ref_loc) "a";
      let r2 = ref "b" in
      set string_ref_loc r2;
      require_equal (module String) !(get string_ref_loc) "b";
      [%expect {| |}]
    ;;

    module Shared = struct
      open Shared

      external unsafe_of_atomic_loc
        : ('a : value_or_null).
        ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
        @@ portable
        = "%identity"

      module Ex = struct
        type t =
          { mutable int_field : int [@atomic]
          ; mutable string_ref_field : string ref [@atomic]
          }

        let mk x y @ shared = { int_field = x; string_ref_field = ref y }
        let int_loc t = unsafe_of_atomic_loc [%atomic.loc t.int_field]
        let string_ref_loc t = unsafe_of_atomic_loc [%atomic.loc t.string_ref_field]
      end

      let get = get
      let set = set

      let%expect_test "get and set" =
        let ex = Ex.mk 1 "a" in
        let int_loc = Ex.int_loc ex in
        require_equal (module Int) (get int_loc) 1;
        set int_loc 2;
        require_equal (module Int) (get int_loc) 2;
        let string_ref_loc = Ex.string_ref_loc ex in
        require_equal (module String) (get string_ref_loc).contents "a";
        [%expect {| |}]
      ;;

      let exchange = exchange

      let%expect_test "exchange" =
        let loc = Ex.mk 1 "a" |> Ex.int_loc in
        let result = exchange loc 2 in
        let new_value = get loc in
        print_s [%message (result : int) (new_value : int)];
        [%expect
          {|
          ((result    1)
           (new_value 2))
          |}]
      ;;

      let compare_and_set = compare_and_set

      let%expect_test "compare_and_set" =
        let loc = Ex.mk 1 "a" |> Ex.int_loc in
        let compare_failed = compare_and_set loc ~if_phys_equal_to:4 ~replace_with:10 in
        let current_value = get loc in
        print_s
          [%message
            (compare_failed : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
        [%expect
          {|
          ((compare_failed Compare_failed)
           (current_value  1))
          |}];
        let set_here = compare_and_set loc ~if_phys_equal_to:1 ~replace_with:10 in
        let current_value = get loc in
        print_s
          [%message
            (set_here : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
        [%expect
          {|
          ((set_here      Set_here)
           (current_value 10))
          |}]
      ;;

      let compare_exchange = compare_exchange

      let%expect_test "compare_exchange" =
        let loc = Ex.mk 1 "a" |> Ex.int_loc in
        let result = compare_exchange loc ~if_phys_equal_to:4 ~replace_with:10 in
        let current_value = get loc in
        print_s [%message (result : int) (current_value : int)];
        [%expect
          {|
          ((result        1)
           (current_value 1))
          |}];
        let result = compare_exchange loc ~if_phys_equal_to:1 ~replace_with:10 in
        let current_value = get loc in
        print_s [%message (result : int) (current_value : int)];
        [%expect
          {|
          ((result        1)
           (current_value 10))
          |}];
        [%expect {| |}]
      ;;

      let update = update
      let update_and_return = update_and_return

      let%expect_test "update and update_and_return" =
        let ex = Ex.mk 1 "a" in
        let loc = Ex.int_loc ex in
        let result = update_and_return loc ~pure_f:(fun x -> x + 1) in
        let new_value = get loc in
        print_s [%message (result : int) (new_value : int)];
        [%expect
          {|
          ((result    1)
           (new_value 2))
          |}]
      ;;

      let%expect_test "update doesn't allocate" =
        let ex = Ex.mk 1 "a" in
        let loc = Ex.int_loc ex in
        require_no_allocation (fun () -> update loc ~pure_f:(fun x -> x + 1))
      ;;

      let incr = incr
      let decr = decr
      let fetch_and_add = fetch_and_add
      let add = add
      let sub = sub
      let logand = logand
      let logor = logor
      let logxor = logxor

      let%expect_test "int ops" =
        let ex = Ex.mk 0 "a" in
        let loc = Ex.int_loc ex in
        incr loc;
        require_equal (module Int) (get loc) 1;
        decr loc;
        require_equal (module Int) (get loc) 0;
        require_equal (module Int) (fetch_and_add loc 2) 0;
        require_equal (module Int) (get loc) 2;
        add loc 3;
        require_equal (module Int) (get loc) 5;
        sub loc 2;
        require_equal (module Int) (get loc) 3;
        logand loc 5;
        require_equal (module Int) (get loc) 1;
        logor loc 6;
        require_equal (module Int) (get loc) 7;
        logxor loc 5;
        require_equal (module Int) (get loc) 2;
        [%expect {| |}]
      ;;
    end
  end
end

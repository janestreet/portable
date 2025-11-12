open! Base
module Backoff = Basement.Stdlib_shim.Backoff
module Compare_failed_or_set_here = Basement.Compare_failed_or_set_here
include Basement.Subatomic

let make_alone =
  if Basement.Stdlib_shim.runtime5 ()
  then make_contended
  else
    (* [caml_atomic_make_contended] is not supported on runtime4; we can just fall back to
       regular make, which is semantically correct and we shouldn't be as worried about
       false sharing on single-core applications anyway. *)
    make
;;

module
  [@inline] Make (Impl : sig
  @@ portable
    type (!'a : value_or_null) t

    val get : ('a : value_or_null). 'a t @ local -> 'a
    val set : ('a : value_or_null). 'a t @ local -> 'a -> unit

    module Shared : sig
      val get : ('a : value_or_null). 'a t @ local shared -> 'a @ shared
      val set : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> unit
      val exchange : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> 'a

      val compare_and_set
        : ('a : value_or_null mod contended).
        'a t @ local shared -> 'a -> 'a -> bool

      val compare_exchange
        : ('a : value_or_null mod contended).
        'a t @ local shared -> 'a -> 'a -> 'a

      val fetch_and_add : int t @ local shared -> int -> int
      val add : int t @ local shared -> int -> unit
      val sub : int t @ local shared -> int -> unit
      val logand : int t @ local shared -> int -> unit
      val logor : int t @ local shared -> int -> unit
      val logxor : int t @ local shared -> int -> unit
      val incr : int t @ local shared -> unit
      val decr : int t @ local shared -> unit
    end
  end) =
struct
  open Impl

  type (!'a : value_or_null) t = 'a Impl.t

  module Nonatomic = struct
    let get = get
    let set = set

    let exchange t x =
      let result = get t in
      set t x;
      result
    ;;

    let compare_and_set t ~if_phys_equal_to ~replace_with
      : Basement.Compare_failed_or_set_here.t
      =
      if phys_equal (get t) if_phys_equal_to
      then (
        set t replace_with;
        Set_here)
      else Compare_failed
    ;;

    let compare_exchange t ~if_phys_equal_to ~replace_with =
      let old_value = get t in
      if phys_equal old_value if_phys_equal_to then set t replace_with;
      old_value
    ;;

    let rec update_and_return t ~pure_f =
      let old_value = get t in
      let new_value = pure_f old_value in
      match compare_and_set t ~if_phys_equal_to:old_value ~replace_with:new_value with
      | Set_here -> old_value
      | Compare_failed -> update_and_return t ~pure_f
    ;;

    let update (type a : value_or_null) (t : a t) ~pure_f =
      let _ : a = update_and_return t ~pure_f in
      ()
    ;;

    let fetch_and_add t n =
      let result = get t in
      set t (result + n);
      result
    ;;

    let add t n = set t (get t + n)
    let sub t n = set t (get t - n)
    let logand t n = set t (get t land n)
    let logor t n = set t (get t lor n)
    let logxor t n = set t (get t lxor n)
    let incr n = add n 1
    let decr n = sub n 1
  end

  module Shared = struct
    include Shared

    let[@inline] compare_and_set t ~if_phys_equal_to ~replace_with
      : Compare_failed_or_set_here.t
      =
      match compare_and_set t if_phys_equal_to replace_with with
      | false -> Compare_failed
      | true -> Set_here
    ;;

    let[@inline] compare_exchange t ~if_phys_equal_to ~replace_with =
      compare_exchange t if_phys_equal_to replace_with
    ;;

    let[@inline] update_and_return t ~pure_f =
      let[@inline] rec aux backoff =
        let old = get t in
        let new_ = pure_f old in
        match compare_and_set t ~if_phys_equal_to:old ~replace_with:new_ with
        | Set_here -> old
        | Compare_failed -> aux (Backoff.once backoff)
      in
      aux Backoff.default [@nontail]
    ;;

    let[@inline] update t ~pure_f =
      Basement.Stdlib_shim.ignore_contended (update_and_return t ~pure_f)
    ;;
  end

  module%template Atomic = struct
    open Shared

    [@@@synchro.default sync]

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

  include Nonatomic
  include Atomic
end

include Make (Basement.Subatomic)

let equal equal_a t1 t2 = equal_a (get t1) (get t2)
let sexp_of_t sexp_of_a t = sexp_of_a (get t)
let t_of_sexp a_of_sexp sexp = make (a_of_sexp sexp)

module Loc = struct
  include Make (Basement.Subatomic.Loc)

  external unsafe_of_atomic_loc
    : ('a : value_or_null).
    ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
    @@ portable
    = "%identity"

  module Shared = struct
    include Shared

    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
      @@ portable
      = "%identity"
  end

  let sexp_of_t sexp_of_a t = sexp_of_a (get t)
end

module Private = struct
  include Subatomic_intf.Definitions
end

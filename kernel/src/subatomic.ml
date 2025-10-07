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

module Shared = struct
  include Shared

  external compare_and_set
    : ('a : value_or_null mod contended).
    'a t @ local shared
    -> if_phys_equal_to:'a
    -> replace_with:'a
    -> Compare_failed_or_set_here.t
    @@ portable
    = "%atomic_cas"

  external compare_exchange
    : ('a : value_or_null mod contended).
    'a t @ local shared -> if_phys_equal_to:'a -> replace_with:'a -> 'a
    @@ portable
    = "%atomic_compare_exchange"

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

let equal equal_a t1 t2 = equal_a (get t1) (get t2)
let sexp_of_t sexp_of_a t = sexp_of_a (get t)
let t_of_sexp a_of_sexp sexp = make (a_of_sexp sexp)

module Loc = struct
  include Loc

  module Shared = struct
    include Shared

    external compare_and_set
      : ('a : value_or_null mod contended).
      'a t @ local shared
      -> if_phys_equal_to:'a
      -> replace_with:'a
      -> Compare_failed_or_set_here.t
      @@ portable
      = "%atomic_cas_loc"

    external compare_exchange
      : ('a : value_or_null mod contended).
      'a t @ local shared -> if_phys_equal_to:'a -> replace_with:'a -> 'a
      @@ portable
      = "%atomic_compare_exchange_loc"

    let update_and_return t ~pure_f =
      let rec aux backoff =
        let old = get t in
        let new_ = pure_f old in
        match compare_and_set t ~if_phys_equal_to:old ~replace_with:new_ with
        | Set_here -> old
        | Compare_failed -> aux (Backoff.once backoff)
          [@@inline]
      in
      aux Backoff.default [@nontail]
    [@@inline]
    ;;

    let update t ~pure_f =
      Basement.Stdlib_shim.ignore_contended (update_and_return t ~pure_f)
    [@@inline]
    ;;
  end

  let sexp_of_t sexp_of_a t = sexp_of_a (get t)
end

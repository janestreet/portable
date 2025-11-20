open! Base
open Basement

module Compare_failed_or_set_here = struct
  type t = Basement.Compare_failed_or_set_here.t =
    | Compare_failed
    | Set_here
  [@@deriving sexp_of ~stackify]
end

type ('a : value_or_null) t = 'a Basement.Portable_atomic.t

let make_alone =
  if Basement.Stdlib_shim.runtime5 ()
  then Basement.Portable_atomic.make_contended
  else
    (* [caml_atomic_make_contended] is not supported on runtime4; we can just fall back to
       regular make, which is semantically correct and we shouldn't be as worried about
       false sharing on single-core applications anyway. *)
    Basement.Portable_atomic.make
;;

let[@inline] make ?(padded = false) value =
  if padded then make_alone value else Basement.Portable_atomic.make value
;;

external get
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable
  @@ portable
  = "%atomic_load"

external exchange
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable -> 'a @ contended portable
  @@ portable
  = "%atomic_exchange"

external set
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable -> unit
  @@ portable
  = "%atomic_set"

external compare_and_set
  : ('a : value_or_null).
  ('a t[@local_opt])
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> Compare_failed_or_set_here.t
  @@ portable
  = "%atomic_cas"

external compare_exchange
  : ('a : value_or_null).
  ('a t[@local_opt])
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> 'a @ contended portable
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

let[@inline] update (type a : value_or_null) (t : a t) ~pure_f =
  Basement.Stdlib_shim.ignore_contended (update_and_return t ~pure_f : a)
;;

external fetch_and_add
  :  (int t[@local_opt])
  -> int
  -> int
  @@ portable
  = "%atomic_fetch_add"

external add : (int t[@local_opt]) -> int -> unit @@ portable = "%atomic_add"
external sub : (int t[@local_opt]) -> int -> unit @@ portable = "%atomic_sub"
external logand : (int t[@local_opt]) -> int -> unit @@ portable = "%atomic_land"
external logor : (int t[@local_opt]) -> int -> unit @@ portable = "%atomic_lor"
external logxor : (int t[@local_opt]) -> int -> unit @@ portable = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1
let sexp_of_t sexp_of_a t = sexp_of_a (get t)
let t_of_sexp a_of_sexp sexp = make (a_of_sexp sexp)

module Loc = struct
  type ('a : value_or_null mod contended portable) t = 'a Stdlib.Atomic.Loc.t

  external get
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a @ contended
    @@ portable
    = "%atomic_load_loc"

  external set
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a @ contended -> unit
    @@ portable
    = "%atomic_set_loc"

  external exchange
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a @ contended -> 'a @ contended
    @@ portable
    = "%atomic_exchange_loc"

  external compare_and_set
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended
    -> Compare_failed_or_set_here.t
    @@ portable
    = "%atomic_cas_loc"

  external compare_exchange
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended
    -> 'a @ contended
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

  external fetch_and_add
    :  (int t[@local_opt]) @ contended
    -> int
    -> int
    @@ portable
    = "%atomic_fetch_add_loc"

  external add
    :  (int t[@local_opt]) @ contended
    -> int
    -> unit
    @@ portable
    = "%atomic_add_loc"

  external sub
    :  (int t[@local_opt]) @ contended
    -> int
    -> unit
    @@ portable
    = "%atomic_sub_loc"

  external logand
    :  (int t[@local_opt]) @ contended
    -> int
    -> unit
    @@ portable
    = "%atomic_land_loc"

  external logor
    :  (int t[@local_opt]) @ contended
    -> int
    -> unit
    @@ portable
    = "%atomic_lor_loc"

  external logxor
    :  (int t[@local_opt]) @ contended
    -> int
    -> unit
    @@ portable
    = "%atomic_lxor_loc"

  let incr r = add r 1
  let decr r = sub r 1
  let sexp_of_t sexp_of_a t = sexp_of_a (get t)
end

let contents (t : _ t) = [%atomic.loc t.contended.Stdlib.Atomic.contents]

module Expert = struct
  (* This is subject to CSE. *)
  external fenceless_get_cse
    : ('a : value_or_null).
    ('a t[@local_opt]) -> 'a @ contended portable
    @@ portable
    = "%field0"

  let[@inline] fenceless_get t =
    (* We use [Sys.opaque_identity] to prevent CSE. *)
    fenceless_get_cse (Sys.opaque_identity t)
  ;;
end

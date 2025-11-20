module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here
open! Base

(* This module encapsulates the fact that ['a Atomic_array] is actually represented as ['a
   Uniform_array.t] internally. All the rest of the functions in this module are
   guaranteed to only be implemented in terms of this Impl module, to avoid accidentally
   relying on the internal representation. This ensures, importantly, that all accesses
   are done atomically, since the memory model does not allow mixing atomic and nonatomic
   operations on a single memory location *)
module Impl : sig
  type (!'a : value_or_null) t : value mod contended portable

  val create : ('a : value_or_null). len:int -> 'a @ contended portable -> 'a t

  val init
    : ('a : value_or_null).
    int -> f:(int -> 'a @ contended portable) @ local -> 'a t

  val length : ('a : value_or_null). 'a t @ local -> int

  val of_list
    : ('a : value_or_null).
    'a list @ contended portable -> 'a t @ contended portable

  val unsafe_get : ('a : value_or_null). 'a t @ local -> int -> 'a @ contended portable

  val unsafe_set
    : ('a : value_or_null).
    'a t @ local -> int -> 'a @ contended portable -> unit

  val unsafe_exchange
    : ('a : value_or_null).
    'a t @ local -> int -> 'a @ contended portable -> 'a @ contended portable

  val unsafe_compare_and_set
    : ('a : value_or_null).
    'a t @ local
    -> int
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended portable
    -> Compare_failed_or_set_here.t

  val unsafe_compare_exchange
    : ('a : value_or_null).
    'a t @ local
    -> int
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended portable
    -> 'a @ contended portable

  val unsafe_fetch_and_add : int t @ local -> int -> int -> int
  val unsafe_add : int t @ local -> int -> int -> unit
  val unsafe_sub : int t @ local -> int -> int -> unit
  val unsafe_land : int t @ local -> int -> int -> unit
  val unsafe_lor : int t @ local -> int -> int -> unit
  val unsafe_lxor : int t @ local -> int -> int -> unit
end = struct
  type ('a : value_or_null) t : value mod contended portable =
    { arr : 'a portended Uniform_array.t }
  [@@unboxed] [@@unsafe_allow_any_mode_crossing]

  let create ~len a = { arr = Uniform_array.create ~len { portended = a } }
  let init len ~f = { arr = Uniform_array.init len ~f:(fun i -> { portended = f i }) }

  external portended_wrap_list
    : ('a : value_or_null).
    'a list @ contended portable -> 'a portended list
    @@ portable
    = "%identity"

  let of_list l = { arr = Uniform_array.of_list (portended_wrap_list l) }
  let length { arr } = Uniform_array.length arr

  external unsafe_get
    : ('a : value_or_null).
    local_ 'a t -> int -> 'a @ contended portable
    @@ portable
    = "%atomic_load_field"

  external unsafe_set
    : ('a : value_or_null).
    local_ 'a t -> int -> 'a @ contended portable -> unit
    @@ portable
    = "%atomic_set_field"

  external unsafe_exchange
    : ('a : value_or_null).
    local_ 'a t -> int -> 'a @ contended portable -> 'a @ contended portable
    @@ portable
    = "%atomic_exchange_field"

  external unsafe_compare_and_set
    : ('a : value_or_null).
    local_ 'a t
    -> int
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended portable
    -> Compare_failed_or_set_here.t
    @@ portable
    = "%atomic_cas_field"

  external unsafe_compare_exchange
    : ('a : value_or_null).
    local_ 'a t
    -> int
    -> if_phys_equal_to:'a @ contended
    -> replace_with:'a @ contended portable
    -> 'a @ contended portable
    @@ portable
    = "%atomic_compare_exchange_field"

  external unsafe_fetch_and_add
    :  local_ int t
    -> int
    -> int
    -> int
    @@ portable
    = "%atomic_fetch_add_field"

  external unsafe_add
    :  local_ int t
    -> int
    -> int
    -> unit
    @@ portable
    = "%atomic_add_field"

  external unsafe_sub
    :  local_ int t
    -> int
    -> int
    -> unit
    @@ portable
    = "%atomic_sub_field"

  external unsafe_land
    :  local_ int t
    -> int
    -> int
    -> unit
    @@ portable
    = "%atomic_land_field"

  external unsafe_lor
    :  local_ int t
    -> int
    -> int
    -> unit
    @@ portable
    = "%atomic_lor_field"

  external unsafe_lxor
    :  local_ int t
    -> int
    -> int
    -> unit
    @@ portable
    = "%atomic_lxor_field"
end

include Impl

let[@inline] check_index t index function_name =
  if index < 0 || index >= length t
  then invalid_arg ("Atomic_array." ^ function_name ^ ": index out of bounds")
;;

let get t index =
  check_index t index "get";
  unsafe_get t index
;;

let set t index value =
  check_index t index "set";
  unsafe_set t index value
;;

let exchange t index value =
  check_index t index "exchange";
  unsafe_exchange t index value
;;

let compare_and_set t index ~if_phys_equal_to ~replace_with =
  check_index t index "compare_and_set";
  unsafe_compare_and_set t index ~if_phys_equal_to ~replace_with
;;

let compare_exchange t index ~if_phys_equal_to ~replace_with =
  check_index t index "compare_exchange";
  unsafe_compare_exchange t index ~if_phys_equal_to ~replace_with
;;

let fetch_and_add t index n =
  check_index t index "fetch_and_add";
  unsafe_fetch_and_add t index n
;;

let add t index n =
  check_index t index "add";
  unsafe_add t index n
;;

let sub t index n =
  check_index t index "sub";
  unsafe_sub t index n
;;

let logand t index n =
  check_index t index "logand";
  unsafe_land t index n
;;

let logor t index n =
  check_index t index "logor";
  unsafe_lor t index n
;;

let logxor t index n =
  check_index t index "logxor";
  unsafe_lxor t index n
;;

let incr t index = add t index 1
let decr t index = sub t index 1

let to_list t =
  let n = length t in
  let[@inline] rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum else loop (i - 1) (get t (i - 1) :: accum)
  in
  loop n [] [@nontail]
;;

let sexp_of_t sexp_of_a t =
  Sexp.List (List.init (length t) ~f:(fun i -> sexp_of_a (get t i)))
;;

let t_of_sexp (type a : value_or_null mod portable) (a_of_sexp : _ -> a) sexp =
  sexp |> [%of_sexp: a list] |> of_list
;;

let%template[@mode m = (global, local)] compare
  (type a : value_or_null mod contended)
  compare_a
  (t1 : a t @ m)
  (t2 : a t @ m)
  =
  let len1 = length t1 in
  let len2 = length t2 in
  let len_cmp = Int.compare len1 len2 in
  if not (Int.equal len_cmp 0)
  then len_cmp
  else (
    let[@inline] rec loop i =
      if i >= len1
      then 0
      else (
        let cmp = compare_a (get t1 i) (get t2 i) in
        if Int.equal cmp 0 then loop (i + 1) else cmp)
    in
    loop 0 [@nontail])
;;

let%template[@mode m = (global, local)] equal
  (type a : value_or_null mod contended)
  equal_a
  (t1 : a t @ m)
  (t2 : a t @ m)
  =
  let len1 = length t1 in
  let len2 = length t2 in
  if not (Int.equal len1 len2)
  then false
  else (
    let[@inline] rec loop i =
      if i >= len1
      then true
      else if equal_a (get t1 i) (get t2 i)
      then loop (i + 1)
      else false
    in
    loop 0 [@nontail])
;;

let quickcheck_generator (type a : value_or_null mod portable) quickcheck_generator_a =
  let open Base_quickcheck.Export in
  [%quickcheck.generator: a list]
  |> Base_quickcheck.Generator.map ~f:(fun (l : a list) -> of_list l)
;;

let quickcheck_observer (type a : value_or_null mod contended) quickcheck_observer_a =
  let open Base_quickcheck.Export in
  [%quickcheck.observer: a list]
  |> Base_quickcheck.Observer.unmap ~f:(fun (t : a t) -> to_list t)
;;

let quickcheck_shrinker
  (type a : value_or_null mod contended portable)
  quickcheck_shrinker_a
  =
  let open Base_quickcheck.Export in
  [%quickcheck.shrinker: a list]
  |> Base_quickcheck.Shrinker.map
       ~f:(fun (l : a list) -> of_list l)
       ~f_inverse:(fun (t : a t) -> to_list t)
;;

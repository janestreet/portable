module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here
module Atomic_ = Atomic
open! Base
module Atomic = Atomic_

(* This module encapsulates the fact that ['a Atomic_array] is actually represented as ['a
   Uniform_array.t] internally. All the rest of the functions in this module are
   guaranteed to only be implemented in terms of this Impl module, to avoid accidentally
   relying on the internal representation. This ensures, importantly, that all accesses
   are done atomically, since the memory model does not allow mixing atomic and nonatomic
   operations on a single memory location *)
module Impl : sig
  type !'a t

  val create : 'a. len:int -> 'a -> 'a t
  val init : 'a. int -> f:(int -> 'a) -> 'a t
  val length : 'a. 'a t -> int
  val of_list : 'a. 'a list -> 'a t
  val unsafe_get : 'a. 'a t -> int -> 'a
  val unsafe_set : 'a. 'a t -> int -> 'a -> unit
  val unsafe_exchange : 'a. 'a t -> int -> 'a -> 'a

  val unsafe_compare_and_set
    : 'a.
    'a t -> int -> if_phys_equal_to:'a -> replace_with:'a -> Compare_failed_or_set_here.t

  val unsafe_compare_exchange
    : 'a.
    'a t -> int -> if_phys_equal_to:'a -> replace_with:'a -> 'a

  val unsafe_fetch_and_add : int t -> int -> int -> int
  val unsafe_add : int t -> int -> int -> unit
  val unsafe_sub : int t -> int -> int -> unit
  val unsafe_land : int t -> int -> int -> unit
  val unsafe_lor : int t -> int -> int -> unit
  val unsafe_lxor : int t -> int -> int -> unit
end = struct
  type 'a t = { inner : 'a Atomic.t Array.t }
  [@@unboxed] [@@unsafe_allow_any_mode_crossing]

  let create ~len a = { inner = Array.init len ~f:(fun _ -> Atomic.make a) }
  let init len ~f = { inner = Array.init len ~f:(fun i -> Atomic.make (f i)) }

  let of_list = function
    | [] -> { inner = [||] }
    | hd :: tl as l ->
      let a = Array.create ~len:(List.length l) (Atomic.make hd) in
      let rec fill i = function
        | [] -> a
        | hd :: tl ->
          Array.unsafe_set a i (Atomic.make hd);
          fill (i + 1) tl
      in
      { inner = fill 1 tl }
  ;;

  let length t = Array.length t.inner
  let unsafe_get t i = Atomic.get (Array.unsafe_get t.inner i)
  let unsafe_set t i v = Atomic.set (Array.unsafe_get t.inner i) v
  let unsafe_exchange t i v = Atomic.exchange (Array.unsafe_get t.inner i) v

  let unsafe_compare_and_set t i ~if_phys_equal_to ~replace_with =
    Atomic.compare_and_set (Array.unsafe_get t.inner i) ~if_phys_equal_to ~replace_with
  ;;

  let unsafe_compare_exchange t i ~if_phys_equal_to ~replace_with =
    Atomic.compare_exchange (Array.unsafe_get t.inner i) ~if_phys_equal_to ~replace_with
  ;;

  let unsafe_fetch_and_add t i v = Atomic.fetch_and_add (Array.unsafe_get t.inner i) v
  let unsafe_add t i v = Atomic.add (Array.unsafe_get t.inner i) v
  let unsafe_sub t i v = Atomic.sub (Array.unsafe_get t.inner i) v
  let unsafe_land t i v = Atomic.logand (Array.unsafe_get t.inner i) v
  let unsafe_lor t i v = Atomic.logor (Array.unsafe_get t.inner i) v
  let unsafe_lxor t i v = Atomic.logxor (Array.unsafe_get t.inner i) v
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

let t_of_sexp (type a) (a_of_sexp : _ -> a) sexp = sexp |> [%of_sexp: a list] |> of_list

let%template[@mode m = (global, local)] compare (type a) compare_a (t1 : a t) (t2 : a t) =
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

let%template[@mode m = (global, local)] equal (type a) equal_a (t1 : a t) (t2 : a t) =
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

let quickcheck_generator (type a) quickcheck_generator_a =
  let open Base_quickcheck.Export in
  [%quickcheck.generator: a list]
  |> Base_quickcheck.Generator.map ~f:(fun (l : a list) -> of_list l)
;;

let quickcheck_observer (type a) quickcheck_observer_a =
  let open Base_quickcheck.Export in
  [%quickcheck.observer: a list]
  |> Base_quickcheck.Observer.unmap ~f:(fun (t : a t) -> to_list t)
;;

let quickcheck_shrinker (type a) quickcheck_shrinker_a =
  let open Base_quickcheck.Export in
  [%quickcheck.shrinker: a list]
  |> Base_quickcheck.Shrinker.map
       ~f:(fun (l : a list) -> of_list l)
       ~f_inverse:(fun (t : a t) -> to_list t)
;;

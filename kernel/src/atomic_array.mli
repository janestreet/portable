@@ portable

module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here
open Base

(** Represents an array of atomic locations.

    This is essentially a ['a Core.Atomic.t array], except it avoids the extra indirection
    of an atomic ref at each index. *)
type !'a t : value mod contended portable

[%%rederive:
  type nonrec (!'a : value mod contended) t = 'a t
  [@@deriving sexp_of, compare ~localize, equal ~localize]]

[%%rederive: type nonrec (!'a : value mod portable) t = 'a t [@@deriving of_sexp]]

[%%rederive:
  type nonrec (!'a : value mod contended portable) t = 'a t [@@deriving quickcheck]]

(** [create ~len value] creates a new array of [n] atomic locations having given [value]. *)
val create : len:int -> 'a @ contended portable -> 'a t

(** [init n ~f] returns a fresh atomic array of length [n], with element number [i]
    initialized to the result of [f i]. *)
val init : int -> f:(int -> 'a @ contended portable) @ local -> 'a t

(** [of_list l] returns a fresh atomic array containing the elements of the list [l]. *)
val of_list : 'a list @ contended portable -> 'a t

(** [to_list atomic_array] returns a list containing the elements of the atomic array
    [atomic_array]. The loads of the elements of the array to populate the list are done
    atomically. *)
val to_list : 'a t @ local -> 'a list @ contended portable

(** [length atomic_array] returns the length of the [atomic_array]. *)
val length : 'a t @ local -> int

(** [get atomic_array index] reads and returns the value at the specified [index] of the
    [atomic_array]. Raises [Invalid_argument] if [index] is out of bounds. *)
val get : 'a t @ local -> int -> 'a @ contended portable

(** [set atomic_array index value] writes the given [value] to the specified [index] of
    the [atomic_array]. Raises [Invalid_argument] if [index] is out of bounds. *)
val set : 'a t @ local -> int -> 'a @ contended portable -> unit

(** [exchange atomic_array index value] sets the value at [index] to [value], and returns
    the previous value. Raises [Invalid_argument] if [index] is out of bounds. *)
val exchange : 'a t @ local -> int -> 'a @ contended portable -> 'a @ contended portable

(** [compare_and_set atomic_array index ~if_phys_equal_to ~replace_with] atomically
    updates the specified [index] of the [atomic_array] to [replace_with] only if its
    current value is physically equal to [if_phys_equal_to] -- the comparison and the set
    occur atomically. Returns [Set_here] if the update was successful, or [Compare_failed]
    otherwise. Raises [Invalid_argument] if [index] is out of bounds. *)
val compare_and_set
  :  'a t @ local
  -> int
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> Compare_failed_or_set_here.t

(** [compare_exchange atomic_array index ~if_phys_equal_to ~replace_with] sets the new
    value at [index] to [replace_with] only if its current value is physically equal to
    [if_phys_equal_to] -- the comparison and the set occur atomically. Returns the
    previous value at [index], or the current (unchanged) value if the comparison failed.
    Raises [Invalid_argument] if [index] is out of bounds. *)
val compare_exchange
  :  'a t @ local
  -> int
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> 'a @ contended portable

(** [fetch_and_add atomic_array index n] atomically increments the value at [index] by
    [n], and returns the previous value (before the increment). Raises [Invalid_argument]
    if [index] is out of bounds. *)
val fetch_and_add : int t @ local -> int -> int -> int

(** [add atomic_array index i] atomically adds [i] to the value at [index]. Raises
    [Invalid_argument] if [index] is out of bounds. *)
val add : int t @ local -> int -> int -> unit

(** [sub atomic_array index i] atomically subtracts [i] from the value at [index]. Raises
    [Invalid_argument] if [index] is out of bounds. *)
val sub : int t @ local -> int -> int -> unit

(** [logand atomic_array index i] atomically bitwise-ands [i] onto the value at [index].
    Raises [Invalid_argument] if [index] is out of bounds. *)
val logand : int t @ local -> int -> int -> unit

(** [logor atomic_array index i] atomically bitwise-ors [i] onto the value at [index].
    Raises [Invalid_argument] if [index] is out of bounds. *)
val logor : int t @ local -> int -> int -> unit

(** [logxor atomic_array index i] atomically bitwise-xors [i] onto the value at [index].
    Raises [Invalid_argument] if [index] is out of bounds. *)
val logxor : int t @ local -> int -> int -> unit

(** [incr atomic_array index] atomically increments the value at [index] by [1]. Raises
    [Invalid_argument] if [index] is out of bounds. *)
val incr : int t @ local -> int -> unit

(** [decr atomic_array index] atomically decrements the value at [index] by [1]. Raises
    [Invalid_argument] if [index] is out of bounds. *)
val decr : int t @ local -> int -> unit

(** Unsafe versions that do not perform bounds checking *)

val unsafe_get : 'a t @ local -> int -> 'a @ contended portable
val unsafe_set : 'a t @ local -> int -> 'a @ contended portable -> unit

val unsafe_exchange
  :  'a t @ local
  -> int
  -> 'a @ contended portable
  -> 'a @ contended portable

val unsafe_compare_and_set
  :  'a t @ local
  -> int
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> Compare_failed_or_set_here.t

val unsafe_compare_exchange
  :  'a t @ local
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

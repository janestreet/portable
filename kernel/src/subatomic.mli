(** A [Subatomic.t] is a hybrid between [ref] and [Atomic.t]. When you have [uncontended]
    accces to a subatomic, you can do non-atomic reads/writes to it, like a [ref]. When
    you have [shared] access to a subatomic, you can do atomic reads/writes to it, like an
    [Atomic]. Nothing can be done to a [contended] subatomic.

    The safety of this interface depends on two differences from each of its counterparts:
    - Unlike [ref], you cannot non-atomically read a [shared] subatomic
    - Unlike [Atomic.t], you cannot do anything with a [contended] subatomic, and
      subatomics consequently do not mode cross on the contention axis

    Conceptually, ['a Atomic.t] could be implemented as a globally-[shared]
    ['a Subatomic.t]:
    {[
      type 'a isolated =
        | Isolated : ('a, 'k) Capsule.Data.t * 'k Capsule.Key.t -> 'a isolated
      [@@unboxed]

      type 'a atomic = 'a subatomic isolated
    ]} *)

type !'a t = 'a Basement.Subatomic.t

[%%rederive: type nonrec !'a t = 'a t [@@deriving equal, sexp_of]]
[%%rederive: type nonrec !'a t = 'a t [@@deriving of_sexp]]

(** Create a subatomic reference; has the same codegen as constructing an [Atomic.t] or a
    [ref]. *)
external make : 'a. 'a -> ('a t[@local_opt]) = "%makemutable"

(** Create a subatomic reference that is alone on a cache line. See {!Atomic.make_alone}
    for details. *)
val make_alone : 'a. 'a -> 'a t

(** [get t] performs an uncontended non-atomic read. *)
val get : 'a. 'a t -> 'a

(** [set t x] performs an uncontended non-atomic write. *)
val set : 'a. 'a t -> 'a -> unit

module Shared : sig
  (** [get t] performs a shared atomic read. *)
  val get : 'a. 'a t -> 'a

  (** [set t x] performs a shared atomic write. *)
  val set : 'a. 'a t -> 'a -> unit

  (** [exchange t v] sets the value of [t] to [v], and returns the previous value *)
  val exchange : 'a. 'a t -> 'a -> 'a

  (** [compare_and_set t ~if_phys_equal_to ~replace_with] sets the new value of [t] to
      [replace_with] {i only} if its current value is physically equal to
      [if_phys_equal_to] -- the comparison and the set occur atomically. Returns
      [Set_here] if the value was set to [replace_with] by this call to [compare_and_set],
      or [Compare_failed] if the current value was not physically equal to
      [if_phys_equal_to] and hence the atomic reference was left unchanged. *)
  val compare_and_set
    : 'a.
    'a t -> if_phys_equal_to:'a -> replace_with:'a -> Atomic.Compare_failed_or_set_here.t

  (** [compare_exchange t ~if_phys_equal_to ~replace_with] sets the new value of [t] to
      [replace_with] only if its current value is physically equal to [if_phys_equal_to]
      -- the comparison and the set occur atomically. Returns the previous value of [t],
      or the current (unchanged) value if the comparison failed. *)
  val compare_exchange : 'a. 'a t -> if_phys_equal_to:'a -> replace_with:'a -> 'a

  (** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
      [pure_f] may be called multiple times, so should be free of side effects. *)
  val update : 'a. 'a t -> pure_f:('a -> 'a) -> unit

  (** [update_and_return t ~pure_f] atomically updates [t] to be the result of
      [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
      effects. Returns the old value. *)
  val update_and_return : 'a. 'a t -> pure_f:('a -> 'a) -> 'a

  (** [fetch_and_add t n] atomically increments the value of [t] by [n], and returns the
      previous value (before the increment). *)
  val fetch_and_add : int t -> int -> int

  (** [add r i] atomically adds [i] to the value of [r]. *)
  val add : int t -> int -> unit

  (** [sub t i] atomically subtracts [i] from the value of [t]. *)
  val sub : int t -> int -> unit

  (** [logand t i] atomically bitwise-ands [i] onto [t]. *)
  val logand : int t -> int -> unit

  (** [logor t i] atomically bitwise-ands [i] onto [t]. *)
  val logor : int t -> int -> unit

  (** [logxor t i] atomically bitwise-xors [i] onto [t]. *)
  val logxor : int t -> int -> unit

  (** [incr t] atomically increments the value of [t] by [1]. *)
  val incr : int t -> unit

  (** [decr t] atomically decrements the value of [t] by [1]. *)
  val decr : int t -> unit
end

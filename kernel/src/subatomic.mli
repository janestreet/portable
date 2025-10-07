@@ portable

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

type (!'a : value_or_null) t : mutable_data with 'a = 'a Basement.Subatomic.t

[%%rederive: type nonrec (!'a : value mod contended) t = 'a t [@@deriving equal, sexp_of]]
[%%rederive: type nonrec (!'a : value mod portable) t = 'a t [@@deriving of_sexp]]

(** Create a subatomic reference; has the same codegen as constructing an [Atomic.t] or a
    [ref]. *)
external make : ('a : value_or_null). 'a -> ('a t[@local_opt]) = "%makemutable"

(** Create a subatomic reference that is alone on a cache line. See {!Atomic.make_alone}
    for details. *)
val make_alone : ('a : value_or_null). 'a -> 'a t

(** [get t] performs an uncontended non-atomic read. *)
val get : ('a : value_or_null). 'a t @ local -> 'a

(** [set t x] performs an uncontended non-atomic write. *)
val set : ('a : value_or_null). 'a t @ local -> 'a -> unit

module Shared : sig
  (** [get t] performs a shared atomic read. *)
  val get : ('a : value_or_null). 'a t @ local shared -> 'a @ shared

  (** [set t x] performs a shared atomic write. *)
  val set : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> unit

  (** [exchange t v] sets the value of [t] to [v], and returns the previous value *)
  val exchange : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> 'a

  (** [compare_and_set t ~if_phys_equal_to ~replace_with] sets the new value of [t] to
      [replace_with] {i only} if its current value is physically equal to
      [if_phys_equal_to] -- the comparison and the set occur atomically. Returns
      [Set_here] if the value was set to [replace_with] by this call to [compare_and_set],
      or [Compare_failed] if the current value was not physically equal to
      [if_phys_equal_to] and hence the atomic reference was left unchanged. *)
  val compare_and_set
    : ('a : value_or_null mod contended).
    'a t @ local shared
    -> if_phys_equal_to:'a
    -> replace_with:'a
    -> Atomic.Compare_failed_or_set_here.t

  (** [compare_exchange t ~if_phys_equal_to ~replace_with] sets the new value of [t] to
      [replace_with] only if its current value is physically equal to [if_phys_equal_to]
      -- the comparison and the set occur atomically. Returns the previous value of [t],
      or the current (unchanged) value if the comparison failed. *)
  val compare_exchange
    : ('a : value_or_null mod contended).
    'a t @ local shared -> if_phys_equal_to:'a -> replace_with:'a -> 'a

  (** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
      [pure_f] may be called multiple times, so should be free of side effects. *)
  val update
    : ('a : value_or_null mod contended).
    'a t @ local shared -> pure_f:('a -> 'a) @ local -> unit

  (** [update_and_return t ~pure_f] atomically updates [t] to be the result of
      [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
      effects. Returns the old value. *)
  val update_and_return
    : ('a : value_or_null mod contended).
    'a t @ local shared -> pure_f:('a -> 'a) @ local -> 'a

  (** [fetch_and_add t n] atomically increments the value of [t] by [n], and returns the
      previous value (before the increment). *)
  val fetch_and_add : int t @ local shared -> int -> int

  (** [add r i] atomically adds [i] to the value of [r]. *)
  val add : int t @ local shared -> int -> unit

  (** [sub t i] atomically subtracts [i] from the value of [t]. *)
  val sub : int t @ local shared -> int -> unit

  (** [logand t i] atomically bitwise-ands [i] onto [t]. *)
  val logand : int t @ local shared -> int -> unit

  (** [logor t i] atomically bitwise-ands [i] onto [t]. *)
  val logor : int t @ local shared -> int -> unit

  (** [logxor t i] atomically bitwise-xors [i] onto [t]. *)
  val logxor : int t @ local shared -> int -> unit

  (** [incr t] atomically increments the value of [t] by [1]. *)
  val incr : int t @ local shared -> unit

  (** [decr t] atomically decrements the value of [t] by [1]. *)
  val decr : int t @ local shared -> unit
end

module Loc : sig
  type (!'a : value_or_null) t : mutable_data with 'a

  [%%rederive: type nonrec (!'a : value mod contended) t = 'a t [@@deriving sexp_of]]

  external unsafe_of_atomic_loc
    : ('a : value_or_null).
    ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
    = "%identity"
  [@@ocaml.doc
    {| Convert an [Atomic.Loc.t] to a [Subatomic.Loc.t]. Currently, there is no compiler
        support for subatomic fields, but we can mimic it using atomic fields.

        Correct usage of this function is to have a record with an atomic field, and to
        only ever access that field like
        {[
          type t = { mutable field : int [@atomic] }

          let get_field t = Subatomic.Loc.unsafe_of_atomic_loc [%atomic.loc t.field]
        ]}

        Accessing or mutating the field in any other way is unsound. |}]

  val get : ('a : value_or_null). local_ 'a t -> 'a
  [@@ocaml.doc
    {| [get [%subatomic.loc r.f]] gets the current value of [r.f] non-atomically. |}]

  val set : ('a : value_or_null). local_ 'a t -> 'a -> unit
  [@@ocaml.doc
    {| [set [%subatomic.loc r.f] v] sets the value of [r.f] to [v] non-atomically. |}]

  module Shared : sig
    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
      = "%identity"
    [@@ocaml.doc
      {| Like {!Subatomic.Loc.unsafe_of_atomic_loc}, but for [shared] values. |}]

    val get : ('a : value_or_null). 'a t @ local shared -> 'a @ shared
    [@@ocaml.doc
      {| [get [%subatomic.loc r.f]] gets the current value of [r.f] atomically. |}]

    val set : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> unit
    [@@ocaml.doc
      {| [set [%subatomic.loc r.f] v] sets the value of [r.f] to [v] atomically.

          Use atomic operations, [update], or [update_and_return] instead of [get]ing the
          value, modifying it in-place, then [set]ing it |}]

    val exchange : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> 'a
    [@@ocaml.doc
      {| [exchange [%subatomic.loc] r.f v] sets the value of [r.f] to [v] atomically, and
          returns the previous value |}]

    val compare_and_set
      : ('a : value_or_null mod contended).
      'a t @ local shared
      -> if_phys_equal_to:'a
      -> replace_with:'a
      -> Atomic.Compare_failed_or_set_here.t
    [@@ocaml.doc
      {| [compare_and_set [%subatomic.loc r.f] ~if_phys_equal_to ~replace_with] sets the
          new value of [r.f] to [replace_with] {i only} if its current value is physically
          equal to [if_phys_equal_to] -- the comparison and the set occur atomically.
          Returns [Set_here] if the value was set to [replace_with] by this call to
          [compare_and_set], or [Compare_failed] if the current value was not physically
          equal to [if_phys_equal_to] and hence the atomic reference was left unchanged. |}]

    val compare_exchange
      : ('a : value_or_null mod contended).
      'a t @ local shared -> if_phys_equal_to:'a -> replace_with:'a -> 'a
    [@@ocaml.doc
      {| [compare_exchange [%subatomic.loc r.f] ~if_phys_equal_to ~replace_with] sets the
          new value of [r.f] to [replace_with] only if its current value is physically
          equal to [if_phys_equal_to] -- the comparison and the set occur atomically.
          Returns the previous value of [r.f], or the current (unchanged) value if the
          comparison failed. |}]

    val update
      : ('a : value_or_null mod contended).
      'a t @ local shared -> pure_f:local_ ('a -> 'a) -> unit
    [@@ocaml.doc
      {| [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
          [pure_f] may be called multiple times, so should be free of side effects. |}]

    val update_and_return
      : ('a : value_or_null mod contended).
      'a t @ local shared -> pure_f:local_ ('a -> 'a) -> 'a
    [@@ocaml.doc
      {| [update_and_return t ~pure_f] atomically updates [t] to be the result of
          [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of
          side effects. Returns the old value. |}]

    val fetch_and_add : int t @ local shared -> int -> int
    [@@ocaml.doc
      {| [fetch_and_add [%subatomic.loc r.f] n] atomically increments the value of [r.f]
          by [n], and returns the previous value (before the increment). |}]

    val add : int t @ local shared -> int -> unit
    [@@ocaml.doc
      {| [add [%subatomic.loc r.f] i] atomically adds [i] to the value of [r.f] |}]

    val sub : int t @ local shared -> int -> unit
    [@@ocaml.doc
      {| [sub [%subatomic.loc r.f] i] atomically subtracts [i] from the value of [r.f] |}]

    val logand : int t @ local shared -> int -> unit
    [@@ocaml.doc
      {| [logand [%subatomic.loc r.f] i] atomically bitwise-ands [i] onto [r.f]. |}]

    val logor : int t @ local shared -> int -> unit
    [@@ocaml.doc
      {| [logor [%subatomic.loc r.f] i] atomically bitwise-ands [i] onto [r.f]. |}]

    val logxor : int t @ local shared -> int -> unit
    [@@ocaml.doc
      {| [logxor [%subatomic.loc r.f] i] atomically bitwise-xors [i] onto [r.f]. |}]

    val incr : int t @ local shared -> unit
    [@@ocaml.doc
      {| [incr [%subatomic.loc r.f]] atomically increments the value of [r.f] by [1]. |}]

    val decr : int t @ local shared -> unit
    [@@ocaml.doc
      {| [decr [%subatomic.loc r.f]] atomically decrements the value of [r.f] by [1]. |}]
  end
end
[@@ocaml.doc
  {| A [Subatomic.Loc.t] is the subatomic analog of [Atomic.Loc.t], i.e. a subatomic
      reference to a field of a record. |}]

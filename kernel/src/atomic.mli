@@ portable

(** An atomic (mutable) reference to a value of type ['a].

    Atomic references mode cross both contention and portability, meaning they are always
    uncontended and always portable, regardless of the kind of the ['a] type parameter, or
    the mode of the atomic reference itself.

    - They are always uncontended because mutating an atomic reference happens
      {i atomically} - multiple fibers mutating the same atomic reference simultaneously
      do not cause a data race.

    - They are always portable because all the functions for creating or mutating an
      atomic reference require the ['a] argument to be provided at the portable mode

    For atomic integers, always mutate their value using one of the intrinsic operations
    ([fetch_and_add], [add], [sub], [logand], [logor], [logxor], [incr], or [decr]). For
    atomic references to complex structures, use [update] to atomically update the atomic.
    For example, to atomically add a value to a [Set]:

    {[
      open! Base
      open! Portable

      let atomically_add_to_set (set_atomic : Set.M(Int).t Atomic.t) value =
        Atomic.update set_atomic ~pure_f:(fun set -> Set.add set value)
      ;;
    ]} *)

module Compare_failed_or_set_here : sig
  (** The result of a call to [compare_and_set]. See the documentation of that function
      for more information. *)
  type t = Basement.Compare_failed_or_set_here.t =
    | Compare_failed
    | Set_here
  [@@deriving sexp_of ~stackify]
end

type (!'a : value_or_null) t : value mod contended portable =
  'a Basement.Portable_atomic.t

[%%rederive: type nonrec (!'a : value mod contended) t = 'a t [@@deriving sexp_of]]
[%%rederive: type nonrec (!'a : value mod portable) t = 'a t [@@deriving of_sexp]]

(** [make v] creates an atomic reference with initial value [v].

    The optional [padded] argument specifies whether or not the atomic reference should be
    padded to cache line size to avoid false sharing. When padded, i.e. [~padded:true], an
    atomic reference occupies 4-16x the memory of one allocated without padding. By
    default [padded] is [false].

    When a CPU core attempts to perform a write, it takes exclusive ownership of the
    entire cache line containing the memory location being written to. This means that
    accessing disjoint memory locations sharing a cache line when at least one of those
    accesses is a write is impossible. This is called false sharing. The cache coherence
    traffic due to repeated invalidations can quickly become very expensive.

    As a general guideline, it is typically beneficial to pad data structures that live
    for a long time and are frequently accessed by multiple CPU cores and frequently
    written to by at least one CPU core. *)
val make
  : ('a : value_or_null).
  ?padded:bool (** default:[false] *) @ local -> 'a @ contended portable -> 'a t

(** [get r] gets the the current value of [r]. *)
external get
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable
  = "%atomic_load"

(** [set r v] sets the value of [r] to [v] *)
external set
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable -> unit
  @@ portable
  = "%atomic_set"

(** [exchange r v] sets the value of [r] to [v], and returns the previous value *)
external exchange
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ contended portable -> 'a @ contended portable
  = "%atomic_exchange"

(** [compare_and_set r ~if_phys_equal_to ~replace_with] sets the new value of [r] to
    [replace_with] {i only} if its current value is physically equal to [if_phys_equal_to]
    -- the comparison and the set occur atomically. Returns [Set_here] if the value was
    set to [replace_with] by this call to [compare_and_set], or [Compare_failed] if the
    current value was not physically equal to [if_phys_equal_to] and hence the atomic
    reference was left unchanged. *)
external compare_and_set
  : ('a : value_or_null).
  ('a t[@local_opt])
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> Compare_failed_or_set_here.t
  = "%atomic_cas"

(** [compare_exchange r ~if_phys_equal_to ~replace_with] sets the new value of [r] to
    [replace_with] only if its current value is physically equal to [if_phys_equal_to] --
    the comparison and the set occur atomically. Returns the previous value of [r], or the
    current (unchanged) value if the comparison failed. *)
external compare_exchange
  : ('a : value_or_null).
  ('a t[@local_opt])
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended portable
  -> 'a @ contended portable
  = "%atomic_compare_exchange"

(** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
    [pure_f] may be called multiple times, so should be free of side effects. *)
val update
  : ('a : value_or_null).
  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ local
  -> unit

(** [update_and_return t ~pure_f] atomically updates [t] to be the result of
    [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
    effects. Returns the old value. *)
val update_and_return
  : ('a : value_or_null).
  'a t @ local
  -> pure_f:('a @ contended portable -> 'a @ contended portable) @ local
  -> 'a @ contended portable

(** [fetch_and_add r n] atomically increments the value of [r] by [n], and returns the
    previous value (before the increment). *)
external fetch_and_add : (int t[@local_opt]) -> int -> int = "%atomic_fetch_add"

(** [add r i] atomically adds [i] to the value of [r]. *)
external add : (int t[@local_opt]) -> int -> unit = "%atomic_add"

(** [sub r i] atomically subtracts [i] from the value of [r]. *)
external sub : (int t[@local_opt]) -> int -> unit = "%atomic_sub"

(** [logand r i] atomically bitwise-ands [i] onto [r]. *)
external logand : (int t[@local_opt]) -> int -> unit = "%atomic_land"

(** [logor r i] atomically bitwise-ands [i] onto [r]. *)
external logor : (int t[@local_opt]) -> int -> unit = "%atomic_lor"

(** [logxor r i] atomically bitwise-xors [i] onto [r]. *)
external logxor : (int t[@local_opt]) -> int -> unit = "%atomic_lxor"

(** [incr r] atomically increments the value of [r] by [1]. *)
val incr : int t @ local -> unit

(** [decr r] atomically decrements the value of [r] by [1]. *)
val decr : int t @ local -> unit

module Loc : sig
  type ('a : value_or_null mod contended portable) t : mutable_data with 'a =
    'a Stdlib.Atomic.Loc.t
  [@@ocaml.doc
    {| A value of type ['a Atomic.Loc.t] represents an "atomic location" - a reference to
      an atomic mutable field within a record. They can be obtained using the
      [[%atomic.loc]] construct - for example, if you have a record like:

      {[
        type t =
          { name : string
          ; mutable count : int [@atomic]
          }
      ]}

      and a value [r : t], you can obtain an [Atomic.Loc.t] pointing to the [count] field
      with the expression [[%atomic.loc r.count]]. You can then use the functions in this
      module to perform atomic operations on that atomic field, for example:

      {[
        let incr_count (r : t) = Atomic.Loc.incr [%atomic.loc r.count]
      ]}

      The functions in this module mirror the functions on atomic refs in [Atomic], and
      the same guidance holds: if you have an atomic mutable field containing an integer,
      you should mutate it using the atomic operations on integers ([fetch_and_add],
      [add], [sub], [logand], [logor], [logxor], [incr], or [decr]). If you have an atomic
      mutable field of some other type, you should use [update] or [update_and_return]. |}]
  [@@deriving sexp_of]

  external get
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a
    = "%atomic_load_loc"
  [@@ocaml.doc {| [get [%atomic.loc r.f]] gets the current value of [r.f]. |}]

  external set
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a -> unit
    = "%atomic_set_loc"
  [@@ocaml.doc
    {| [set [%atomic.loc r.f] v] sets the value of [r.f] to [v].

      Use atomic operations, [update], or [update_and_return] instead of [get]ing the
      value, modifying it in-place, then [set]ing it |}]

  external exchange
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> 'a -> 'a
    = "%atomic_exchange_loc"
  [@@ocaml.doc
    {| [exchange [%atomic.loc] r.f v] sets the value of [r.f] to [v], and returns the
      previous value |}]

  external compare_and_set
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended
    -> if_phys_equal_to:'a
    -> replace_with:'a
    -> Compare_failed_or_set_here.t
    = "%atomic_cas_loc"
  [@@ocaml.doc
    {| [compare_and_set [%atomic.loc r.f] ~if_phys_equal_to ~replace_with] sets the new
      value of [r.f] to [replace_with] {i only} if its current value is physically equal
      to [if_phys_equal_to] -- the comparison and the set occur atomically. Returns
      [Set_here] if the value was set to [replace_with] by this call to [compare_and_set],
      or [Compare_failed] if the current value was not physically equal to
      [if_phys_equal_to] and hence the atomic reference was left unchanged. |}]

  external compare_exchange
    : ('a : value_or_null mod contended portable).
    ('a t[@local_opt]) @ contended -> if_phys_equal_to:'a -> replace_with:'a -> 'a
    = "%atomic_compare_exchange_loc"
  [@@ocaml.doc
    {| [compare_exchange [%atomic.loc r.f] ~if_phys_equal_to ~replace_with] sets the new
      value of [r.f] to [replace_with] only if its current value is physically equal to
      [if_phys_equal_to] -- the comparison and the set occur atomically. Returns the
      previous value of [r.f], or the current (unchanged) value if the comparison failed. |}]

  val update
    : ('a : value_or_null mod contended portable).
    'a t @ contended local -> pure_f:local_ ('a -> 'a) -> unit
  [@@ocaml.doc
    {| [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
      [pure_f] may be called multiple times, so should be free of side effects. |}]

  val update_and_return
    : ('a : value_or_null mod contended portable).
    'a t @ contended local -> pure_f:local_ ('a -> 'a) -> 'a
  [@@ocaml.doc
    {| [update_and_return t ~pure_f] atomically updates [t] to be the result of
      [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
      effects. Returns the old value. |}]

  external fetch_and_add
    :  (int t[@local_opt]) @ contended
    -> int
    -> int
    = "%atomic_fetch_add_loc"
  [@@ocaml.doc
    {| [fetch_and_add [%atomic.loc r.f] n] atomically increments the value of [r.f] by [n],
      and returns the previous value (before the increment). |}]

  external add : (int t[@local_opt]) @ contended -> int -> unit = "%atomic_add_loc"
  [@@ocaml.doc {| [add [%atomic.loc r.f] i] atomically adds [i] to the value of [r.f] |}]

  external sub : (int t[@local_opt]) @ contended -> int -> unit = "%atomic_sub_loc"
  [@@ocaml.doc
    {| [sub [%atomic.loc r.f] i] atomically subtracts [i] from the value of [r.f] |}]

  external logand : (int t[@local_opt]) @ contended -> int -> unit = "%atomic_land_loc"
  [@@ocaml.doc {| [logand [%atomic.loc r.f] i] atomically bitwise-ands [i] onto [r.f]. |}]

  external logor : (int t[@local_opt]) @ contended -> int -> unit = "%atomic_lor_loc"
  [@@ocaml.doc {| [logor [%atomic.loc r.f] i] atomically bitwise-ands [i] onto [r.f]. |}]

  external logxor : (int t[@local_opt]) @ contended -> int -> unit = "%atomic_lxor_loc"
  [@@ocaml.doc {| [logxor [%atomic.loc r.f] i] atomically bitwise-xors [i] onto [r.f]. |}]

  val incr : int t @ contended local -> unit
  [@@ocaml.doc
    {| [incr [%atomic.loc r.f]] atomically increments the value of [r.f] by [1]. |}]

  val decr : int t @ contended local -> unit
  [@@ocaml.doc
    {| [decr [%atomic.loc r.f]] atomically decrements the value of [r.f] by [1]. |}]
end
[@@ocaml.doc {| Atomic "locations" |}]

val contents
  : ('a : value_or_null mod contended).
  'a t -> 'a Basement.Stdlib_shim.Modes.Portable.t Loc.t @ contended
[@@ocaml.doc
  {| [contents t] is a [Loc.t] referring to the contents of the atomic ref [t] |}]

module Expert : sig
  (** Load the value referenced by the given atomic, without using any compiler or
      hardware fences.

      This is dubiously safe, and has no explicit semantics within the OCaml memory
      model - and may do the wrong thing entirely on backends with weak memory models such
      as ARM. Use with caution! *)
  val fenceless_get : ('a : value_or_null). 'a t @ local -> 'a @ contended portable
end

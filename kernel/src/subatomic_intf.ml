module Definitions = struct
  (*_ Every operation on subatomics shows up in three places:
      1) Untemplated at top-level, non-atomic/uncontended
      2) Templated at top-level, atomic/shared
      3) Untemplated under [Shared], atomic/shared

      We define the module type once, generic over these three, in order to avoid
      duplication and keep the interfaces in sync. To make this work, we actually generate
      8 module types, and pick out the ones we want to use. These module types vary on:
      1) What naming to use (i.e. templated or not)
      2) What implementation to use (i.e. atomic or not)
      3) An extra variable for whether contents need to cross contention (only necessary
         due to a limitation in [ppx_template] which prevents tuples from varying over
         multiple axes arbitrarily and not being able to express complex relationships
         between modes)
  *)
  module type%template
    [@synchro.explicit
      name @ __ = (unsync_uncontended, sync_shared)
      , _impl @ maybe_shared = (unsync_uncontended, sync_shared)]
    [@mode.explicit maybe_contended = (uncontended, contended)] S = sig
    type ('a : value_or_null) t

    (*_ Use [name] to determine templating *)
    [@@@synchro.default name]

    (** Read the current value of a subatomic.

        - [get] performs a non-atomic read on an uncontended subatomic.
        - [get [@synchro atomic]] and [Shared.get] perform an atomic read on a shared
          subatomic. *)
    val get : ('a : value_or_null). 'a t @ local maybe_shared -> 'a @ maybe_shared

    (** Update the current value of a subatomic.

        - [set] performs a non-atomic write on an uncontended subatomic.
        - [set [@synchro atomic]] and [Shared.set] perform an atomic write on a shared
          subatomic. *)
    val set
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared -> 'a -> unit

    (** Update the current value of a subatomic, and return the previous value.

        - [update] performs a non-atomic read/write on an uncontended subatomic.
        - [update [@synchro atomic]] and [Shared.update] perform an atomic read/write on a
          shared subatomic. *)
    val exchange
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared -> 'a -> 'a

    (** If the current value of the subatomic is [phys_equal] to [if_phys_equal], then
        update it to [replace_with] and return [Set_here]; otherwise, return
        [Compare_failed] (with the subatomic left unchanged).

        - [compare_and_set] performs a non-atomic read/compare/write on an uncontended
          subatomic.
        - [compare_and_set [@synchro atomic]] and [Shared.compare_and_set] perform an
          atomic read/compare/write on a shared subatomic. *)
    val compare_and_set
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared
      -> if_phys_equal_to:'a
      -> replace_with:'a
      -> Atomic.Compare_failed_or_set_here.t

    (** If the current value of the subatomic is [phys_equal] to [if_phys_equal], then
        update it to [replace_with] and return the previous value; otherwise, return
        current (unchanged) value.

        - [compare_exchange] performs a non-atomic read/compare/write on an uncontended
          subatomic.
        - [compare_exchange [@synchro atomic]] and [Shared.compare_exchange] perform an
          atomic read/compare/write on a shared subatomic. *)
    val compare_exchange
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared -> if_phys_equal_to:'a -> replace_with:'a -> 'a

    (** Update [t] to be the result of [pure_f (get t)]; if [t] is changed before the
        whole operation is complete, retry until success. [pure_f] may be called multiple
        times, so should be free of side effects.

        - [update] performs a non-atomic read/compare/write on an uncontended subatomic.
          [pure_f] still may be called multiple times due to nonportable threading or if
          [pure_f] is not actually pure.
        - [update [@synchro atomic]] and [Shared.update] perform an atomic
          read/compare/write on a shared subatomic. *)
    val update
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared -> pure_f:('a -> 'a) @ local -> unit

    (** Update [t] to be the result of [pure_f (get t)], and return the old value; if [t]
        is changed before the whole operation is complete, retry until success. [pure_f]
        may be called multiple times, so should be free of side effects.

        - [update_and_return] performs a non-atomic read/compare/write on an uncontended
          subatomic. [pure_f] still may be called multiple times due to nonportable
          threading or if [pure_f] is not actually pure.
        - [update_and_return [@synchro atomic]] and [Shared.update_and_return] perform an
          atomic read/compare/write on a shared subatomic. *)
    val update_and_return
      : ('a : value_or_null mod maybe_contended).
      'a t @ local maybe_shared -> pure_f:('a -> 'a) @ local -> 'a

    (** Increments the value of a subatomic by the given value, and return the previous
        value (before the increment).

        - [fetch_and_add] performs a non-atomic update on an uncontended subatomic.
        - [fetch_and_add [@synchro atomic]] and [Shared.fetch_and_add] perform an atomic
          update on a shared subatomic. *)
    val fetch_and_add : int t @ local maybe_shared -> int -> int

    (** Add the given value to the subatomic.

        - [add] performs a non-atomic update on an uncontended subatomic.
        - [add [@synchro atomic]] and [Shared.add] perform an atomic update on a shared
          subatomic. *)
    val add : int t @ local maybe_shared -> int -> unit

    (** Subtract the given value from the subatomic.

        - [sub] performs a non-atomic update on an uncontended subatomic.
        - [sub [@synchro atomic]] and [Shared.sub] perform an atomic update on a shared
          subatomic. *)
    val sub : int t @ local maybe_shared -> int -> unit

    (** Apply bitwise-and of the given value to the subatomic.

        - [logand] performs a non-atomic update on an uncontended subatomic.
        - [logand [@synchro atomic]] and [Shared.logand] perform an atomic update on a
          shared subatomic. *)
    val logand : int t @ local maybe_shared -> int -> unit

    (** Apply bitwise-or of the given value to the subatomic.

        - [logor] performs a non-atomic update on an uncontended subatomic.
        - [logor [@synchro atomic]] and [Shared.logor] perform an atomic update on a
          shared subatomic. *)
    val logor : int t @ local maybe_shared -> int -> unit

    (** Apply bitwise-xor of the given value to the subatomic.

        - [logxor] performs a non-atomic update on an uncontended subatomic.
        - [logxor [@synchro atomic]] and [Shared.logxor] perform an atomic update on a
          shared subatomic. *)
    val logxor : int t @ local maybe_shared -> int -> unit

    (** Increment a subatomic by 1.

        - [incr] performs a non-atomic update on an uncontended subatomic.
        - [incr [@synchro atomic]] and [Shared.incr] perform an atomic update on a shared
          subatomic. *)
    val incr : int t @ local maybe_shared -> unit

    (** Decrement a subatomic by 1.

        - [decr] performs a non-atomic update on an uncontended subatomic.
        - [decr [@synchro atomic]] and [Shared.decr] perform an atomic update on a shared
          subatomic. *)
    val decr : int t @ local maybe_shared -> unit
  end
end

module type Subatomic = sig @@ portable
  open Definitions

  (** A [Subatomic.t] is a hybrid between [ref] and [Atomic.t]. When you have
      [uncontended] access to a subatomic, you can do non-atomic reads/writes to it, like
      a [ref]. When you have [shared] access to a subatomic, you can do atomic
      reads/writes to it, like an [Atomic]. Nothing can be done to a [contended]
      subatomic.

      The safety of this interface depends on two differences from each of its
      counterparts:
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

  [%%rederive:
    type nonrec (!'a : value mod contended) t = 'a t [@@deriving equal, sexp_of]]

  [%%rederive: type nonrec (!'a : value mod portable) t = 'a t [@@deriving of_sexp]]

  (** Create a subatomic reference; has the same codegen as constructing an [Atomic.t] or
      a [ref]. *)
  external make : ('a : value_or_null). 'a -> ('a t[@local_opt]) = "%makemutable"

  (** Create a subatomic reference that is alone on a cache line. See {!Atomic.make_alone}
      for details. *)
  val make_alone : ('a : value_or_null). 'a -> 'a t

  (*_ Untemplated nonatomic uncontended operations, e.g. [Subatomic.get] *)
    include%template
      S
      [@synchro.explicit unsync unsync] [@mode.explicit uncontended]
      with type ('a : value_or_null) t := 'a t
    (** @inline *)

  (*_ Templated atomic shared operations, e.g. [Subatomic.get [@synchro atomic]] *)
    include%template
      S
      [@synchro.explicit sync sync] [@mode.explicit contended]
      with type ('a : value_or_null) t := 'a t
    (** @inline *)

  module%template Shared : sig
    (*_ Untemplated atomic shared operations, e.g. [Subatomic.Shared.get] *)
    include
      S
      [@synchro.explicit unsync sync] [@mode.explicit contended]
      with type ('a : value_or_null) t := 'a t
    (** @inline *)
  end

  module Loc : sig
    type (!'a : value_or_null) t : mutable_data with 'a = 'a Basement.Subatomic.Loc.t

    [%%rederive: type nonrec (!'a : value mod contended) t = 'a t [@@deriving sexp_of]]

    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
      = "%identity"
    [@@ocaml.doc
      {| Convert an [Atomic.Loc.t] to a [Subatomic.Loc.t]. Currently, there is no
          compiler support for subatomic fields, but we can mimic it using atomic fields.

          Correct usage of this function is to have a record with an atomic field, and to
          only ever access that field like
          {[
            type t = { mutable field : int [@atomic] }

            let get_field t = Subatomic.Loc.unsafe_of_atomic_loc [%atomic.loc t.field]
          ]}

          Accessing or mutating the field in any other way is unsound. |}]

    [%%template:
      include
        S
        [@synchro.explicit unsync unsync] [@mode.explicit uncontended]
        with type ('a : value_or_null) t := 'a t
      [@@ocaml.doc {| @inline |}]]

    [%%template:
      include
        S
        [@synchro.explicit sync sync] [@mode.explicit contended]
        with type ('a : value_or_null) t := 'a t
      [@@ocaml.doc {| @inline |}]]

    [%%template:
      module Shared : sig
        external unsafe_of_atomic_loc
          : ('a : value_or_null).
          ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
          = "%identity"
        [@@ocaml.doc
          {| Like {!Subatomic.Loc.unsafe_of_atomic_loc}, but for [shared] values. |}]

        include
          S
          [@synchro.explicit unsync sync] [@mode.explicit contended]
          with type ('a : value_or_null) t := 'a t
        [@@ocaml.doc {| @inline |}]
      end]
  end
  [@@ocaml.doc
    {| A [Subatomic.Loc.t] is the subatomic analog of [Atomic.Loc.t], i.e. a subatomic
        reference to a field of a record. |}]

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)

  module Private : sig
    include module type of struct
      include Definitions
    end
  end
end

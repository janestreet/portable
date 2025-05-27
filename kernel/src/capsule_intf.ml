open! Base
module Capsule = Basement.Capsule

module Definitions = struct
  module type With_mutex = sig
    type k

    val mutex : k Capsule.Mutex.t
  end
end

(** For now, see [Basement.Capsule] for documentation of capsule types.

    This module provides an interface to capsules that is a small subset of
    [Basement.Capsule], starting with the most common entry points. The interface is also
    cleaned up to be somewhat easier to use, and more consistent with other conventions in
    [Base].

    Over time we will provide more of [Basement.Capsule]'s functionality. *)
module type Capsule = sig
  include module type of struct
    include Definitions
  end

  module Password : sig
    type 'k t = 'k Capsule.Password.t
  end

  module Data : sig
    type ('a, 'k) t = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) -> ('a, 'k) t

    (** Retrieve a value using the state stored in a capsule. *)
    val get : 'a 'k 'b. ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> 'b

    (** Like [get], for types that do not cross portability and contention. *)
    val get_contended : ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> 'b

    (** A constrained form of [get] specialized to return [unit]. *)
    val iter : ('a, 'k) t -> f:('a -> unit) -> password:'k Password.t -> unit

    (** These functions enable more complicated manipulation of capsules. *)

    val return : 'a -> ('a, 'k) t
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
    val fst : ('a * _, 'k) t -> ('a, 'k) t
    val snd : (_ * 'b, 'k) t -> ('b, 'k) t
    val map : ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> ('b, 'k) t

    val bind
      :  ('a, 'k1) t
      -> f:('a -> ('b, 'k2) t)
      -> password:'k1 Password.t
      -> ('b, 'k2) t

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : 'a 'k. ('a, 'k) t -> 'a

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a, 'k) t -> 'a
  end

  module Mutex : sig
    type 'k t = 'k Capsule.Mutex.t

    type packed = Capsule.Mutex.packed = P : 'k t -> packed
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : With_mutex

    val with_lock : 'k t -> f:('k Password.t -> 'a) -> 'a
  end

  module Isolated : sig
    (** A value isolated within its own capsule.

        A primary use-case for this type is to use aliasing as a proxy for contention.
        [unique] access to an ['a Capsule.Isolated.t] allows [uncontended] access to the
        underlying ['a]. [aliased] access to an ['a Capsule.Isolated.t] allows [shared]
        access to the underlying ['a].

        Importantly, since uniqueness is being used to track contention, the contents of a
        ['a t] are necessarily aliased, so having a ['a t @ unique] does not allow you to
        get ['a @ unique]. *)
    type 'a t

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.Isolated.t]
        containing the result. *)
    val create : (unit -> 'a) -> 'a t

    (** [with_unique t ~f] takes a [unique] isolated capsule [t], calls [f] with its
        value, and returns a tuple of the unique isolated capsule and the result of [f]. *)
    val with_unique : 'a 'b. 'a t -> f:('a -> 'b) -> 'a t * 'b Modes.Aliased.t

    (** Like [with_unique], but with the most general mode annotations. *)
    val with_unique_gen : 'a t -> f:('a -> 'b) -> 'a t * 'b

    (** [with_unique t ~f] takes an [aliased] isolated capsule [t], calls [f] with shared
        access to its value, and returns a tuple of the unique isolated capsule and the
        result of [f]. *)
    val with_shared : 'a 'b. 'a t -> f:('a -> 'b) -> 'b

    (** Like [with_shared], but with the most general mode annotations. *)
    val with_shared_gen : 'a 'b. 'a t -> f:('a -> 'b) -> 'b

    (** [unwrap t ~f] takes a [unique] isolated capsule [t] and returns the underlying
        value, merging the capsule with the current capsule. *)
    val%template unwrap : 'a t -> 'a
    [@@mode l = (global, local)]

    (** Project out a contended reference to the underlying value from a unique [t],
        returning the unique [t] back alongside the alias to the underlying value. *)
    val get_id_contended : 'a. 'a t -> 'a t * 'a Modes.Aliased.t
  end

  module Initial : sig
    (** The initial capsule, i.e. the implicit capsule associated with the initial domain.
        This is the capsule in which library top-levels run, and so [nonportable]
        top-level functions are allowed to access it. *)

    (** The brand for the initial capsule. *)
    type k = Capsule.initial

    module Data : sig
      (** A value in the initial capsule. *)
      type 'a t = ('a, k) Data.t [@@deriving sexp]

      (** Store a value in a [Capsule.Data.t] for the initial capsule. This function is
          [nonportable], requiring it to be run from the initial domain. *)
      val%template wrap : 'a -> 'a t
      [@@mode l = (global, local)]

      (** Extract a value from a [Capsule.Data.t] for the initial capsule. This function
          is [nonportable], requiring it to be run from the initial domain. *)
      val%template unwrap : 'a t -> 'a
      [@@mode l = (global, local)]
    end
  end

  module Expert = Basement.Capsule
end

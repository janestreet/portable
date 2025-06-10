open! Base
module Capsule = Basement.Capsule

module Definitions = struct
  module type Module_with_mutex = sig
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
module type Capsule = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Password : sig
    type 'k t : value mod contended portable = 'k Capsule.Password.t
  end

  module Data : sig
    type ('a, 'k) t : value mod contended portable = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) @ local once portable -> ('a, 'k) t

    (** Retrieve a value using the state stored in a capsule. *)
    val get
      : 'a 'k ('b : value mod contended portable).
      ('a, 'k) t
      -> f:('a -> 'b) @ local once portable
      -> password:'k Password.t @ local
      -> 'b

    (** Like [get], for types that do not cross portability and contention. *)
    val get_contended
      :  ('a, 'k) t
      -> f:('a -> 'b @ contended portable) @ local once portable
      -> password:'k Password.t @ local
      -> 'b @ contended portable

    (** A constrained form of [get] specialized to return [unit]. *)
    val iter
      :  ('a, 'k) t
      -> f:('a -> unit) @ local once portable
      -> password:'k Password.t @ local
      -> unit

    (** These functions enable more complicated manipulation of capsules. *)

    val return : ('a : value mod contended) @ portable -> ('a, 'k) t
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
    val fst : ('a * _, 'k) t -> ('a, 'k) t
    val snd : (_ * 'b, 'k) t -> ('b, 'k) t

    val map
      :  ('a, 'k) t
      -> f:('a -> 'b) @ local once portable
      -> password:'k Password.t @ local
      -> ('b, 'k) t

    val bind
      :  ('a, 'k1) t
      -> f:('a -> ('b, 'k2) t) @ local once portable
      -> password:'k1 Password.t @ local
      -> ('b, 'k2) t

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : ('a : value mod contended portable) 'k. ('a, 'k) t -> 'a

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a : value mod portable, 'k) t -> 'a @ contended
  end

  module Mutex : sig
    type 'k t = 'k Capsule.Mutex.t
    type packed = Capsule.Mutex.packed = P : 'k t -> packed [@@unboxed]

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : Module_with_mutex

    val with_lock : 'k t -> f:('k Password.t @ local -> 'a) @ local once -> 'a
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
    type 'a t : value mod contended portable

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.Isolated.t]
        containing the result. *)
    val create : (unit -> 'a) @ local once portable -> 'a t @ unique

    (** [with_unique t ~f] takes a [unique] isolated capsule [t], calls [f] with its
        value, and returns a tuple of the unique isolated capsule and the result of [f]. *)
    val with_unique
      : 'a ('b : value mod contended portable).
      'a t @ unique
      -> f:('a -> 'b) @ local once portable
      -> 'a t * 'b Modes.Aliased.t @ unique

    (** Like [with_unique], but with the most general mode annotations. *)
    val with_unique_gen
      :  'a t @ unique
      -> f:('a -> 'b @ contended portable unique) @ local once portable
      -> 'a t * 'b @ contended portable unique

    (** [with_unique t ~f] takes an [aliased] isolated capsule [t], calls [f] with shared
        access to its value, and returns a tuple of the unique isolated capsule and the
        result of [f]. *)
    val with_shared
      : ('a : value mod portable) ('b : value mod contended portable).
      'a t -> f:('a @ shared -> 'b) @ local once portable -> 'b

    (** Like [with_shared], but with the most general mode annotations. *)
    val with_shared_gen
      : ('a : value mod portable) 'b.
      'a t
      -> f:('a @ shared -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** [unwrap t ~f] takes a [unique] isolated capsule [t] and returns the underlying
        value, merging the capsule with the current capsule. *)
    val%template unwrap : 'a t @ l unique -> 'a @ l
    [@@mode l = (global, local)]

    (** Project out a contended reference to the underlying value from a unique [t],
        returning the unique [t] back alongside the alias to the underlying value. *)
    val get_id_contended
      : ('a : value mod portable).
      'a t @ unique -> 'a t * 'a Modes.Aliased.t @ contended unique
  end

  module With_mutex : sig
    type ('a, 'k) inner =
      { data : ('a, 'k) Data.t
      ; mutex : 'k Mutex.t
      }

    (** An ['a Capsule.With_mutex.t] is a value of type ['a] in its own capsule, protected
        by a mutex *)
    type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.With_mutex.t]
        containing the result *)
    val create : (unit -> 'a) @ local once portable -> 'a t

    (** [of_isolated isolated] creates a [Capsule.With_mutex.t] from a value in an
        isolated capsule, consuming the isolated capsule. *)
    val of_isolated : 'a Isolated.t @ unique -> 'a t

    (** [get t ~f] locks the mutex associated with [t] and calls [f] on the protected
        value, returning the result. The result type must be portable and cross
        contention, as it is leaving the capsule associated with the mutex. *)
    val get
      : 'a ('b : value mod contended).
      'a t -> f:('a -> 'b @ portable) @ local once portable -> 'b

    module Guard : sig
      type ('a, 'k) inner =
        { password : 'k Password.t
        ; data : ('a, 'k) Data.t @@ global
        }

      (** A value of this type represents a currently-locked [Capsule.With_mutex.t] *)
      type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]
    end

    (** [with_lock t] locks the mutex associated with [t] and calls [f] with a [Guard.t]
        providing access to the capsule. *)
    val with_lock : 'a t -> f:('a Guard.t @ local -> 'b) @ local once -> 'b

    (** [map t ~f] locks the mutex associated with [t] and calls [f] on the protected
        value, returning a new [With_mutex.t] containing the result in the same capsule,
        and protected by the same mutex. *)
    val map : 'a t -> f:('a -> 'b) @ local once portable -> 'b t

    (** [destroy t] poisons the mutex associated with [t], merging the protected value
        into the current capsule and returning it. *)
    val destroy : 'a t -> 'a
  end

  module (Initial @@ nonportable) : sig
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
      val%template wrap : 'a @ l -> 'a t @ l
      [@@mode l = (global, local)]

      (** Extract a value from a [Capsule.Data.t] for the initial capsule. This function
          is [nonportable], requiring it to be run from the initial domain. *)
      val%template unwrap : 'a t @ l -> 'a @ l
      [@@mode l = (global, local)]
    end
  end

  module Expert = Basement.Capsule
end

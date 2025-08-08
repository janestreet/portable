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
module type Capsule = sig
  include module type of struct
    include Definitions
  end

  module Access : sig
    type 'k t = 'k Capsule.Access.t
    type packed = Capsule.Access.packed = P : 'k t -> packed [@@unboxed]

    (** Obtain an [Access.t] for the current capsule. Since we do not know the brand for
        the current capsule, we receive a fresh one. *)
    val current : unit -> packed
  end

  module Data : sig
    type ('a, 'k) t = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) -> ('a, 'k) t

    [%%template:
    [@@@mode.default l = (global, local)]

    val wrap : access:'k Access.t -> 'a -> ('a, 'k) t
    val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a

    (** These functions enable more complicated manipulation of capsules. *)

    val return : 'a -> ('a, 'k) t
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
    val fst : ('a * _, 'k) t -> ('a, 'k) t
    val snd : (_ * 'b, 'k) t -> ('b, 'k) t

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : 'a 'k. ('a, 'k) t -> 'a

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a, 'k) t -> 'a]
  end

  module Mutex : sig
    type 'k t = 'k Capsule.Mutex.t
    type packed = Capsule.Mutex.packed = P : 'k t -> packed [@@unboxed]

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : Module_with_mutex

    val with_lock : 'k t -> f:('k Access.t -> 'a) -> 'a
  end

  module Isolated : sig
    type ('a, 'k) inner =
      { key : 'k Capsule.Key.t
      ; data : ('a, 'k) Data.t
      }

    (** A value isolated within its own capsule.

        A primary use-case for this type is to use aliasing as a proxy for contention.
        [unique] access to an ['a Capsule.Isolated.t] allows [uncontended] access to the
        underlying ['a]. [aliased] access to an ['a Capsule.Isolated.t] allows [shared]
        access to the underlying ['a].

        Importantly, since uniqueness is being used to track contention, the contents of a
        ['a t] are necessarily aliased, so having a ['a t @ unique] does not allow you to
        get ['a @ unique]. *)
    type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

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

  module Guard : sig
    type ('a, 'k) inner =
      { data : ('a, 'k) Data.t
      ; password : 'k Capsule.Password.t
      }

    (** A locally-protected value within the current capsule.

        A value of type ['a Guard.t] provides [uncontended] access to the underlying ['a]
        as long as the ['a Guard.t] is [local]. *)
    type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [with a ~f] calls [f] with a [local] {!Guard.t} representing local access to [a]
        within the current capsule. *)
    val with_ : 'a -> f:('a t -> 'b) -> 'b

    (** Retrieve a value using data stored locally in the current capsule. *)
    val get : 'a 'b. 'a t -> f:('a -> 'b) -> 'b

    (** Like [get], for result types which do not cross portability and contention *)
    val get_contended : 'a t -> f:('a -> 'b) -> 'b

    (** A constrained version of [get] for functions which return [unit] *)
    val iter : 'a t -> f:('a -> unit) -> unit

    (** Construct a new local [t] by mapping a function over the contained value *)
    val map : 'a t -> f:('a -> 'b) -> 'b t
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
    val create : (unit -> 'a) -> 'a t

    (** [of_isolated isolated] creates a [Capsule.With_mutex.t] from a value in an
        isolated capsule, consuming the isolated capsule. *)
    val of_isolated : 'a Isolated.t -> 'a t

    (** [with_lock t ~f] locks the mutex associated with [t] and calls [f] on the
        protected value, returning the result. *)
    val with_lock : 'a t -> f:('a -> 'b) -> 'b

    (** [iter t ~f] is [with_lock t ~f], specialised to a function that returns [unit] *)
    val iter : 'a t -> f:('a -> unit) -> unit

    (** [map t ~f] locks the mutex associated with [t] and calls [f] on the protected
        value, returning a new [With_mutex.t] containing the result in the same capsule,
        and protected by the same mutex. *)
    val map : 'a t -> f:('a -> 'b) -> 'b t

    (** [destroy t] poisons the mutex associated with [t], merging the protected value
        into the current capsule and returning it. *)
    val destroy : 'a t -> 'a
  end

  module Initial : sig
    (** The initial capsule, i.e. the implicit capsule associated with the initial domain.
        This is the capsule in which library top-levels run, and so [nonportable]
        top-level functions are allowed to access it. *)

    (** The brand for the initial capsule. *)
    type k = Capsule.initial

    (** Access to the initial capsule *)
    val access : k Access.t

    (** [with_access_opt ~f] calls [f (Some Initial.access)] if run on the initial domain,
        or [f None] otherwise. *)
    val%template with_access_opt : f:(k Access.t option -> 'r) -> 'r
    [@@alloc a @ l = (heap_global, stack_local)]

    module Data : sig
      (** A value in the initial capsule. *)
      type 'a t = ('a, k) Data.t [@@deriving sexp]

      [%%template:
      [@@@mode.default l = (global, local)]

      (** Store a value in a [Capsule.Data.t] for the initial capsule. This function is
          [nonportable], requiring it to be run from the initial domain. *)
      val wrap : 'a -> 'a t

      (** Extract a value from a [Capsule.Data.t] for the initial capsule. This function
          is [nonportable], requiring it to be run from the initial domain. *)
      val unwrap : 'a t -> 'a]

      [%%template:
      [@@@alloc.default a @ l = (heap_global, stack_local)]

      (** Attempt to extract a value from a [Capsule.Data.t] for the initial capsule by
          passing it to a function [f] (which must return a portable value), if running on
          the initial domain. If not run on the initial domain, returns [None]. *)
      val get_opt : 'a t -> f:('a -> 'b) -> 'b option

      (** If called on the initial domain, [if_on_initial t ~f] calls [f] with the
          contents of [t]. Otherwise, it does nothing. *)
      val if_on_initial : 'a t -> f:('a -> unit) -> unit

      (** If running on the initial domain, calls [f] with the value inside a
          [Capsule.Initial.Data.t]. Otherwise, raises. *)
      val get_exn : 'a t -> f:('a -> 'b) -> 'b

      (** A version of [get_exn] specialized to functions that return [unit] *)
      val iter_exn : 'a t -> f:('a -> unit) -> unit]
    end
  end

  module Expert = Basement.Capsule
end

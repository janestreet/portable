open! Base
module Capsule = Basement.Capsule

(** For now, see [Basement.Capsule] for documentation of capsule types.

    This module provides an interface to capsules that is a small subset of
    [Basement.Capsule], starting with the most common entry points. The interface is also
    cleaned up to be somewhat easier to use, and more consistent with other conventions in
    [Base].

    Over time we will provide more of [Basement.Capsule]'s functionality. *)
module type Capsule = sig @@ portable
  module Access : sig
    type 'k t : void mod aliased external_ global many portable = 'k Capsule.Access.t
    type packed = Capsule.Access.packed = P : 'k t -> packed [@@unboxed]

    type 'k boxed : value mod aliased external_ global many portable =
      'k Capsule.Access.boxed

    (** Obtain an [Access.t] for the current capsule. Since we do not know the brand for
        the current capsule, we receive a fresh one. *)
    val current : unit -> packed @@ portable

    val unbox : 'k boxed -> 'k t @@ portable
    val box : 'k t -> 'k boxed @@ portable
  end

  module Data : sig
    type ('a, 'k) t : value mod contended portable = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) @ local once portable -> ('a, 'k) t

    [%%template:
    [@@@mode.default l = (global, local)]

    val wrap : access:'k Access.t -> 'a @ l -> ('a, 'k) t @ l
    val unwrap : access:'k Access.t -> ('a, 'k) t @ l -> 'a @ l

    (** These functions enable more complicated manipulation of capsules. *)

    val return : ('a : value mod contended) @ l portable -> ('a, 'k) t @ l
    val both : ('a, 'k) t @ l -> ('b, 'k) t @ l -> ('a * 'b, 'k) t @ l
    val fst : ('a * _, 'k) t @ l -> ('a, 'k) t @ l
    val snd : (_ * 'b, 'k) t @ l -> ('b, 'k) t @ l

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : ('a : value mod contended portable) 'k. ('a, 'k) t @ l -> 'a @ l

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a : value mod portable, 'k) t @ l -> 'a @ contended l]
  end

  module Isolated : sig
    type ('a, 'k) inner : value mod contended portable =
      { key : 'k Capsule.Key.t @@ global
      ; data : ('a, 'k) Data.t @@ aliased
      }

    (** A value isolated within its own capsule.

        A primary use-case for this type is to use aliasing as a proxy for contention.
        [unique] access to an ['a Capsule.Isolated.t] allows [uncontended] access to the
        underlying ['a]. [aliased] access to an ['a Capsule.Isolated.t] allows [shared]
        access to the underlying ['a].

        Importantly, since uniqueness is being used to track contention, the contents of a
        ['a t] are necessarily aliased, so having a ['a t @ unique] does not allow you to
        get ['a @ unique]. *)
    type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

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

    (** [unwrap_shared t ~f] takes an [aliased] isolated capsule [t] and returns the
        underlying value at [shared]. *)
    val%template unwrap_shared : ('a : value mod portable). 'a t @ l -> 'a @ l shared
    [@@mode l = (global, local)]

    (** Project out a contended reference to the underlying value from a unique [t],
        returning the unique [t] back alongside the alias to the underlying value. *)
    val get_id_contended
      : ('a : value mod portable).
      'a t @ unique -> 'a t * 'a Modes.Aliased.t @ contended unique
  end

  module Guard : sig
    type ('a, 'k) inner : value mod contended portable =
      { data : ('a, 'k) Data.t @@ global
      ; password : 'k Capsule.Password.t
      }

    (** An encapsulated value accessible for the duration of the current region.

        A value of type ['a Guard.t] provides [uncontended] access to the underlying ['a]
        over a local scope. *)
    type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [with_ a ~f] calls [f] with a [local] {!Guard.t} representing local access to [a],
        which lives in the current capsule. *)
    val with_ : 'a -> f:('a t @ local -> 'b) @ local once -> 'b

    (** [get t ~f] computes a value using the data accessible via [t]. *)
    val get
      : 'a ('b : value mod contended portable).
      'a t @ local -> f:('a -> 'b) @ local once portable -> 'b

    (** Like [get], but for result types that do not cross portability and contention. *)
    val get_contended
      :  'a t @ local
      -> f:('a -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** Like [get], but for for functions that return [unit]. *)
    val iter : 'a t @ local -> f:('a -> unit) @ local once portable -> unit

    (** Construct a new [t] by mapping a function over the referenced value. *)
    val map : 'a t @ local -> f:('a -> 'b) @ local once portable -> 'b t @ local
  end

  module Shared : sig
    type ('a, 'k) inner : value mod contended portable =
      { data : ('a shared, 'k) Data.t @@ global
      ; password : 'k Capsule.Password.Shared.t
      }

    (** An encapsulated value that may be read for the duration of the current region.

        A value of type ['a Shared.t] provides [shared] access to the underlying ['a] over
        a local scope. An [unyielding] ['a Shared.t] can be captured by functions that run
        on other domains. *)
    type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [with_ a ~f] calls [f] with a [local unyielding] {!Shared.t} representing local
        read-only access to [a], which is readable in the current capsule. *)
    val with_
      : ('a : value mod portable) 'b.
      'a @ shared -> f:('a t @ local unyielding -> 'b) @ local once unyielding -> 'b

    (** [get t ~f] computes a value using data accessible via [t]. *)
    val get
      : ('a : value mod portable) ('b : value mod contended portable).
      'a t @ local -> f:('a @ shared -> 'b) @ local once portable -> 'b

    (** Like [get], but for result types that do not cross portability and contention. *)
    val get_contended
      : ('a : value mod portable) 'b.
      'a t @ local
      -> f:('a @ shared -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** Like [get], but for for functions that return [unit]. *)
    val iter
      : ('a : value mod portable).
      'a t @ local -> f:('a @ shared -> unit) @ local once portable -> unit

    (** Construct a new [t] by mapping a function over the referenced value. *)
    val map
      : ('a : value mod portable) ('b : value mod portable).
      'a t @ local -> f:('a @ shared -> 'b @ shared) @ local once portable -> 'b t @ local

    module Uncontended : sig
      (** Like ['a Shared.t], but allows read-only computations to return an uncontended
          result in the current capsule. *)
      type ('a, 'k) t : value mod contended portable = ('a, 'k) inner =
        { data : ('a shared, 'k) Data.t @@ global
        ; password : 'k Capsule.Password.Shared.t
        }

      type ('a, 'b) f =
        { f : 'k. ('a, 'k) t @ local unyielding -> ('b, 'k) Capsule.Data.Shared.t }

      (** [with_ a ~f] calls [f] with a [local unyielding] {!Uncontended.t} representing
          local read-only access to [a], which is readable in the current capsule.

          The result of [f] is a [Capsule.Data.Shared.t], which can be unwrapped in the
          current capsule. *)
      val with_ : 'a @ shared -> ('a, 'b) f @ local once unyielding -> 'b

      (** [get t ~f] computes a value using data accessible via [t]. *)
      val get
        : ('a : value mod portable) 'b 'k.
        ('a, 'k) t @ local
        -> f:('a @ shared -> 'b) @ local once portable
        -> ('b, 'k) Capsule.Data.Shared.t

      (** Construct a new [t] by mapping a function over the referenced value. *)
      val map
        : ('a : value mod portable) ('b : value mod portable) 'k.
        ('a, 'k) t @ local
        -> f:('a @ shared -> 'b @ shared) @ local once portable
        -> ('b, 'k) t @ local
    end
  end

  module (Initial @@ nonportable) : sig
    (** The initial capsule, i.e. the implicit capsule associated with the initial domain.
        This is the capsule in which library top-levels run, and so [nonportable]
        top-level functions are allowed to access it. *)

    (** The brand for the initial capsule. *)
    type k = Capsule.initial

    (** Access to the initial capsule *)
    val access : k Access.boxed

    (** [with_access_opt ~f] calls [f (Some Initial.access)] if run on the initial domain,
        or [f None] otherwise. *)
    val%template with_access_opt
      :  f:(k Access.boxed option @ local -> 'r @ contended l portable)
         @ local once portable unyielding
      -> 'r @ contended l portable
      @@ portable
    [@@alloc a @ l = (heap_global, stack_local)]

    module Data : sig
      (** A value in the initial capsule. *)
      type 'a t = ('a, k) Data.t [@@deriving sexp]

      [%%template:
      [@@@mode.default l = (global, local)]

      (** Store a value in a [Capsule.Data.t] for the initial capsule. This function is
          [nonportable], requiring it to be run from the initial domain. *)
      val wrap : 'a @ l -> 'a t @ l

      (** Extract a value from a [Capsule.Data.t] for the initial capsule. This function
          is [nonportable], requiring it to be run from the initial domain. *)
      val unwrap : 'a t @ l -> 'a @ l]

      [%%template:
      [@@@alloc.default a @ l = (heap_global, stack_local)]

      (** Attempt to extract a value from a [Capsule.Data.t] for the initial capsule by
          passing it to a function [f] (which must return a portable value), if running on
          the initial domain. If not run on the initial domain, returns [None]. *)
      val get_opt
        : ('a : value mod unyielding) 'b.
        'a t @ l
        -> f:('a @ l -> 'b @ contended l portable) @ local once portable unyielding
        -> 'b option @ contended l portable
        @@ portable

      (** If called on the initial domain, [if_on_initial t ~f] calls [f] with the
          contents of [t]. Otherwise, it does nothing. *)
      val if_on_initial
        : ('a : value mod unyielding) 'b.
        'a t @ l -> f:('a @ l -> unit) @ local once portable unyielding -> unit
        @@ portable

      (** If running on the initial domain, calls [f] with the value inside a
          [Capsule.Initial.Data.t]. Otherwise, raises. *)
      val get_exn
        : ('a : value mod unyielding) 'b.
        'a t @ l
        -> f:('a @ l -> 'b @ contended l portable) @ local once portable unyielding
        -> 'b @ contended l portable
        @@ portable

      (** A version of [get_exn] specialized to functions that return [unit] *)
      val iter_exn
        : ('a : value mod unyielding) 'b.
        'a t @ l -> f:('a @ l -> unit) @ local once portable unyielding -> unit
        @@ portable]
    end
  end

  module Expert = Basement.Capsule
end

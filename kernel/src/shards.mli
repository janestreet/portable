@@ portable

(** Similar to ['a DLS.key], but pre-allocates storage for [Multicore.max_domains ()]
    shards. In return, [Shards.get] has lower overhead than [DLS.get]. Slots from
    terminated domains may be reused. *)
type ('a : value mod non_float) t : immutable_data with 'a

(** Allocate storage for [Multicore.max_domains ()] shards. *)
val create : (unit -> 'a) @ local -> 'a t

(** Retrieve the value for the current shard. *)
val%template get : 'a t @ c -> 'a @ c
[@@mode c = (uncontended, contended)]

module Lazy : functor
    (T : sig
       type t : sync_data

       val create : unit -> t @@ stateless
       [@@ocaml.doc {| [create] may be called arbitrarily many times during resizing. |}]
     end)
    -> sig
  @@ portable
  type t : sync_data
  [@@ocaml.doc {| Like ['a t], but lazily expands storage when required. |}]

  val create : unit -> t [@@ocaml.doc {| Allocate storage up to the current shard. |}]

  val get : t @ local -> T.t
  [@@ocaml.doc
    {| Retrieve the value for the current shard, resizing storage if needed. |}]

  val reset : t @ local -> unit
  [@@ocaml.doc {| Clears and resizes storage up to the current shard. |}]

  module Unboxed : sig
    type t : sync_data
    [@@ocaml.doc
      {| Like ['a t], but avoids an indirection by inlining the underlying atomic.

          The intent is to use this as the type of an atomic record field, for example:

          {[
            open! Base
            open! Portable

            module Shards = Portable.Shards.Lazy (struct
                type t = int Atomic.t

                let create () = Atomic.make 100
              end)

            type t =
              { mutable shards : Shards.Unboxed.t [@atomic]
              ; something_else : string
              }

            let use t =
              let shard = Shards.Unboxed.get [%atomic.loc t.shards] in
              Atomic.get shard
            ;;
          ]} |}]

    val create : unit -> t
    [@@ocaml.doc {| Allocate storage for [Domain.self_index ()] domains. |}]

    val get : t Atomic.Loc.t @ local -> T.t
    [@@ocaml.doc
      {| Retrieve the value for the current shard, resizing storage if needed. |}]

    val reset : t Atomic.Loc.t @ local -> unit
    [@@ocaml.doc {| Clears and resizes storage up to the current shard. |}]
  end
end

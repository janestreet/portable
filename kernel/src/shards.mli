(** Similar to ['a DLS.key], but pre-allocates storage for [Multicore.max_domains ()]
    shards. In return, [Shards.get] has lower overhead than [DLS.get]. Slots from
    terminated domains may be reused. *)
type 'a t

(** Allocate storage for [Multicore.max_domains ()] shards. *)
val create : (unit -> 'a) -> 'a t

(** Retrieve the value for the current shard. *)
val%template get : 'a t -> 'a
[@@mode c = (uncontended, contended)]

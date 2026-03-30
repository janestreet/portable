(** Similar to ['a DLS.key], but pre-allocates storage for
    [Domain.recommended_domain_count ()] domains. If there are no additional domains, each
    domain receives exclusive access to its slot. Slots from terminated domains may be
    reused. In return, [Domain_shards.get] has lower overhead than [DLS.get]. *)
type 'a t

(** Allocate storage for [Domain.recommended_domain_count ()] domains. *)
val create : (unit -> 'a) -> 'a t

(** Retrieve the value for the current domain index. *)
val%template get : 'a t -> 'a
[@@mode c = (uncontended, contended)]

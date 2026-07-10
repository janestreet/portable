module Portable_atomic = Atomic
module Domain = Basement.Stdlib_shim.Domain
open Base

type 'a t = 'a Iarray.t

let shards = 1 lsl Int.ceil_log2 (Stdlib.Domain.recommended_domain_count ())
let[@inline] index () = Domain.self_index () land (shards - 1)
let create f = Iarray.init shards ~f:(fun _ -> f ()) [@nontail]

let%template[@inline] get (t : _ t) =
  (* Index cannot exceed [shards]. *)
  (Iarray.unsafe_get [@mode c]) t (index ())
[@@mode c = (uncontended, contended)]
;;

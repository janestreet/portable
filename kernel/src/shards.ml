module Portable_atomic = Atomic
module Domain = Basement.Stdlib_shim.Domain
open Base

type ('a : value mod non_float) t = 'a Iarray.t

let shards = 1 lsl Int.ceil_log2 (Stdlib.Domain.recommended_domain_count ())
let[@inline] index () = Domain.self_index () land (shards - 1)
let create f = Iarray.init shards ~f:(fun _ -> f ()) [@nontail]

let%template[@inline] get (t : _ t) =
  (* Index cannot exceed [shards]. *)
  (Iarray.unsafe_get [@mode c]) t (index ())
[@@mode c = (uncontended, contended)]
;;

module Lazy (T : sig
    type t : sync_data

    val create : unit -> t @@ stateless
  end) =
struct
  module Unboxed = struct
    type t = T.t Iarray.t

    let create () = Iarray.init (Domain.self_index () + 1) ~f:(fun _ -> T.create ())

    let rec grow (t : t Portable_atomic.Loc.t) new_len =
      let storage = Portable_atomic.Loc.get t in
      let old_len = Iarray.length storage in
      if new_len <= old_len
      then storage
      else (
        let expanded =
          Iarray.init new_len ~f:(fun i ->
            if i < old_len then Iarray.unsafe_get storage i else T.create ())
        in
        match
          Portable_atomic.Loc.compare_and_set
            t
            ~if_phys_equal_to:storage
            ~replace_with:expanded
        with
        | Set_here -> expanded
        | Compare_failed -> grow t new_len)
    [@@inline never] [@@loop]
    ;;

    let get (t : t Portable_atomic.Loc.t) =
      let idx = Domain.self_index () in
      let storage =
        let storage = Portable_atomic.Loc.get t in
        if idx < Iarray.length storage then storage else grow t (idx + 1)
      in
      Iarray.unsafe_get storage idx
    [@@inline]
    ;;

    let reset t = Portable_atomic.Loc.set t (create ())
  end

  type t = { mutable unboxed : Unboxed.t [@atomic] }

  let create () = { unboxed = Unboxed.create () }
  let get t = Unboxed.get [%atomic.loc t.unboxed] [@nontail]
  let reset t = Unboxed.reset [%atomic.loc t.unboxed] [@nontail]
end

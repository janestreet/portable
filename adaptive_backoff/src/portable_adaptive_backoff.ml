open Base
open Basement
open Portable_kernel

module Random_key = struct
  type t = int

  let num_bits =
    let assumed_words_per_cache_line_log_2 = 3 in
    let assumed_max_number_of_cpu_cores_log_2 = 8 in
    assumed_max_number_of_cpu_cores_log_2 + assumed_words_per_cache_line_log_2
  ;;
end

let pow2_len = if Stdlib_shim.runtime5 () then 1 lsl Random_key.num_bits else 0
let table = Atomic_array.create ~len:pow2_len 0

let[@inline] index ~random_key =
  (* We could potentially use a bit mixing hash here, but it is not clear whether that is
     preferable to having the user fully pick the key. In some cases users might want to
     have control over the indexing and deterministically avoid collisions for related
     keys such as atomic locations within a single data structure. *)
  random_key land (pow2_len - 1)
;;

(* We don't want to inline this because [Random.int] is not going to be fully inlined and
   partial inlining will most likely increase generated code size without benefits such as
   reducing register pressure. *)
let[@inline never] once ~random_key ~log_scale =
  if Stdlib_shim.runtime5 ()
  then (
    let i = index ~random_key in
    let n_contending_threads = Atomic_array.unsafe_fetch_and_add table i 1 + 1 in
    let n = ref (Random.int (n_contending_threads lsl log_scale)) in
    while 0 <= !n do
      Stdlib_shim.Domain.cpu_relax ();
      n := !n - 1
    done;
    Atomic_array.unsafe_sub table i 1)
  else Stdlib_shim.Domain.cpu_relax ()
;;

let[@inline] once_unless_alone ~random_key ~log_scale =
  if Stdlib_shim.runtime5 ()
  then (
    let i = index ~random_key in
    if Atomic_array.unsafe_get table i <> 0 then once ~random_key ~log_scale)
;;

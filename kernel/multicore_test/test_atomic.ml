open! Core
open Portable_test_helpers

let%expect_test "update from multiple threads" =
  let atomic = Atomic.make [] in
  let num_threads = 5 in
  let start = Barrier.create num_threads in
  let stop = Barrier.create (num_threads + 1) in
  for n = 0 to num_threads - 1 do
    match
      Multicore.spawn
        (fun () ->
          Barrier.await start;
          Atomic.update atomic ~pure_f:(fun l -> n :: l);
          Barrier.await stop)
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  done;
  Barrier.await stop;
  let final_result =
    Atomic.get atomic
    |> Portability_hacks.Cross.Contended.(cross (list infer))
    |> List.sort ~compare:Int.compare
  in
  print_s [%message (final_result : int list)];
  [%expect {| (final_result (0 1 2 3 4)) |}]
;;

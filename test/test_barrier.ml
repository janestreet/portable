open! Base
open Portable
open Expect_test_helpers_base
module Barrier = Portable_test_helpers.Barrier

(* Disable alert on [Domain.Safe.spawn] since we are explicitly testing its behavior. *)
[@@@alert "-unsafe_parallelism"]

let%expect_test "simple barrier protects a ref write" =
  let i = Atomic.make 0 in
  let result1 = Atomic.make (-1) in
  let result2 = Atomic.make (-1) in
  let barrier = Barrier.create 2 in
  let done_barrier = Barrier.create 3 in
  let f result =
    Barrier.await barrier;
    Atomic.incr i;
    Barrier.await barrier;
    Atomic.set result (Atomic.get i);
    Barrier.await done_barrier
  in
  (match Multicore.spawn (fun () -> f result1) () with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  (match Multicore.spawn (fun () -> f result2) () with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  Barrier.await done_barrier;
  (* Both domains should see both writes, since the writes happen-before the barrier and
     the derefs happen-after the barrier *)
  print_s [%message (Atomic.get result1 : int) (Atomic.get result2 : int)];
  [%expect
    {|
    (("Atomic.get result1" 2)
     ("Atomic.get result2" 2))
    |}];
  (* We can re-use the same barrier now that all of the previous domains
     have passed the barrier. *)
  (match Multicore.spawn (fun () -> f result1) () with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  (match Multicore.spawn (fun () -> f result2) () with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  Barrier.await done_barrier;
  print_s [%message (Atomic.get result1 : int) (Atomic.get result2 : int)];
  [%expect
    {|
    (("Atomic.get result1" 4)
     ("Atomic.get result2" 4))
    |}]
;;

open! Base
open Portable
open Await
open Expect_test_helpers_base
module Barrier = Portable_test_helpers.Barrier

let%expect_test "simple barrier protects a ref write" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let%with.tilde.stack s = Concurrent.with_scope conc () in
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
  Concurrent.spawn s ~f:(fun _ _ _ -> f result1);
  Concurrent.spawn s ~f:(fun _ _ _ -> f result2);
  Barrier.await done_barrier;
  (* Both domains should see both writes, since the writes happen-before the barrier and
     the derefs happen-after the barrier *)
  print_s [%message (Atomic.get result1 : int) (Atomic.get result2 : int)];
  [%expect
    {|
    (("Atomic.get result1" 2)
     ("Atomic.get result2" 2))
    |}];
  (* We can re-use the same barrier now that all of the previous domains have passed the
     barrier. *)
  Concurrent.spawn s ~f:(fun _ _ _ -> f result1);
  Concurrent.spawn s ~f:(fun _ _ _ -> f result2);
  Barrier.await done_barrier;
  print_s [%message (Atomic.get result1 : int) (Atomic.get result2 : int)];
  [%expect
    {|
    (("Atomic.get result1" 4)
     ("Atomic.get result2" 4))
    |}]
;;

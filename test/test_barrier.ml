open! Base
open Portable
open Expect_test_helpers_base
module Barrier = Portable_test_helpers.Barrier

(* Disable alert on [Domain.Safe.spawn] since we are explicitly testing its behavior. *)
[@@@alert "-unsafe_parallelism"]

let%expect_test ("simple barrier protects a ref write" [@tags "runtime5-only"]) =
  let i = Atomic.make 0 in
  let barrier = Barrier.create 2 in
  let f () =
    Barrier.await barrier;
    Atomic.incr i;
    Barrier.await barrier;
    Atomic.get i
  in
  let d1 = Domain.Safe.spawn f in
  let d2 = Domain.Safe.spawn f in
  (* Both domains should see both writes, since the writes happen-before the barrier and
     the derefs happen-after the barrier *)
  print_s [%message (Domain.join d1 : int) (Domain.join d2 : int)];
  [%expect
    {|
    (("Domain.join d1" 2)
     ("Domain.join d2" 2))
    |}];
  (* We can re-use the same barrier now that all of the previous domains
     have passed the barrier. *)
  let d3 = Domain.Safe.spawn f in
  let d4 = Domain.Safe.spawn f in
  print_s [%message (Domain.join d3 : int) (Domain.join d4 : int)];
  [%expect
    {|
    (("Domain.join d3" 4)
     ("Domain.join d4" 4))
    |}]
;;

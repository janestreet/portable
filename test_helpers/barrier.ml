open! Base
open! Portable

type t =
  { capacity : int (** The number of domains which can [await] on the barrier *)
  ; passed : int Atomic.t
  (** The number of domains which have entered, but not exited [await] *)
  ; waiters : int Atomic.t
  (** The number of domains which are ready to exit a call to [await] *)
  }

let create capacity =
  { capacity
  ; passed = Atomic.make 0
  ; waiters =
      (* Between rounds, [waiters] is set to [capacity]; we reset it to [0] once all of
         the domains have entered [await], and let it count back up to [capacity] before
         the domains leave [await]. *)
      Atomic.make capacity
  }
;;

let capacity t = t.capacity

(* NOTE: we assume no more than [capacity] domains call [await] at a time.
   This is documented in the mli file. *)

let await { waiters; capacity; passed } =
  (* Awaiting a barrier progresses in two stages:

     1. Each waiter increments [passed], until the last waiter increments it to equal
        [capacity]
     2. Each waiter synchronizes the exit of [await] using a semaphore on [waiters], by
        each incrementing it in lockstep until [waiters] is equal to [capacity]

     This ensures that we only return from [await] once [capacity] waiters have called
     [await], as simultaneously as possible.
  *)
  if Atomic.fetch_and_add passed 1 = capacity - 1
  then (
    (* If [capacity] waiters have called [await] on the barrier, reset [passed] to 0, and
       reset [waiters] to prepare for synchronizing the return from [await] *)
    Atomic.set passed 0;
    Atomic.set waiters 0)
  else
    (* Otherwise, if we were /not/ the [capacity]th waiter, wait for the [capacity]th
       waiter to reset the barrier. *)
    while Atomic.get waiters = capacity do
      Domain.cpu_relax ()
    done;
  (* Finally, each waiter increments [waiters] up to [capacity] *)
  Atomic.incr waiters;
  while Atomic.get waiters < capacity do
    Domain.cpu_relax ()
    (* When each domain now exits from [await], we're back to the initial state setup by
     [create]:

     - [passed] is 0
     - [waiters] is equal to [capacity]
    *)
  done
;;

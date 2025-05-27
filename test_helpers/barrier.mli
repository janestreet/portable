@@ portable

open! Base

(** A barrier is a synchronisation tool.

    A barrier of capacity [n] blocks domains until [n] of them are waiting. Then these [n]
    domains can pass. Then the barrier is reset.

    NOTE: if more than [n] domains try to [await] the barrier at the same time, the
    barrier might reach an invalid state and deadlock. This means, for example, trying to
    use a barrier of capacity 2 to let 6 domains through the barrier in 3 groups of 2 is a
    programmer error.

    This module is useful to make sure that in tests of multi-domain algorithms, multiple
    domains are actually running in parallel.

    For example, the following code:
    {[
      let example nb_domain =
        let printer i () = print_s [%message "Domain spawned" (i : int)] in
        let domains = List.init nb_domain (fun i -> Domain.spawn (printer i)) in
        List.iter Domain.join domains
      ;;
    ]}

    will almost definitely print the messages in order, or almost in order, because
    printing a string is significantly cheaper than spawning a domain.

    On the other hand, using a barrier helps better exercise the nondeterministic behavior
    of multiple domains running simultaneously. The following code will print in a much
    more random order:
    {[
      let example_with_barrier nb_domain =
        let barrier = Barrier.create nb_domain in
        let printer i () =
          Barrier.await barrier;
          print_s [%message "Domain spawned" (i : int)]
        in
        let domains = List.init nb_domain (fun i -> Domain.spawn (printer i)) in
        List.iter Domain.join domains
      ;;
    ]}

    Barriers also enable multiple rounds of awaiting, such that a domain cannot begin a
    new round before all other domains have finished the previous one. This can be easily
    observed by replacing the printer function in the previous example with this one :

    {[
      let printer i () =
        Barrier.await barrier;
        print_s [%message "First round" (i : int)];
        Barrier.await barrier;
        print_s [%message "Second round" (i : int)]
      ;;
    ]} *)

type t : value mod contended portable

(** [create c] returns a barrier of capacity [c]. *)
val create : int -> t

(** Returns the capacity that was originally passed to [create]. *)
val capacity : t -> int

(** A domain calling [await barrier] will only be able to progress past this function once
    the number of domains waiting at the barrier is equal to its capacity. *)
val await : t -> unit

@@ portable

(** An adaptive randomized backoff mechanism for low level atomic operations.

    The idea is to dynamically adapt the backoff period based on the number of threads
    contending for a specific location. This has been seen to outperform randomized
    exponential backoff in many cases. *)

module Random_key : sig
  type t = int

  (** [num_bits] is the number of low order bits actually used from the key. *)
  val num_bits : int
end

(** [once ~random_key ~log_scale] performs a randomized backoff on the given [random_key]
    based on the number of concurrently contending threads [n] (maintained internally) and
    scaled by [n lsl log_scale].

    The [random_key] should ideally have the lowest [Random_key.num_bits] picked randomly
    and should then be used consistently to correspond to a specific atomic location being
    contended on. Collisions should not be catastrophic, but might result in lower
    performance due to unnecessarily long backoff.

    The optimal [log_scale] value depends on both the hardware, e.g. relative cost of a
    [pause] instruction, and the relative cost, e.g. the length of the critical section,
    of the specific atomic operation being attempted. Typical good values have been
    between [8] and [12]. *)
val once : random_key:Random_key.t -> log_scale:int -> unit

(** [once_unless_alone ~random_key ~log_scale] is [once ~random_key ~log_scale] unless it
    appears that no other thread is concurrently performing a backoff with the
    [random_key].

    This is intended for use on read-only operations or operations that successfully end
    up only reading from atomic locations, e.g. when a data structure is empty, and it is
    possible that client code may repeatedly call such an operation, e.g. to poll in a
    tight loop, which can cause a significant increase in cache coherence traffic if there
    are any writers. To avoid the issue, call [once_unless_alone] just before returning
    from such an operation.

    The decision to backoff or not is done without writing to memory. This means that if
    no thread calls {!once} then no thread will perform backoff, which is not what you
    want. *)
val once_unless_alone : random_key:Random_key.t -> log_scale:int -> unit

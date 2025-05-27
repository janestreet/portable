@@ portable

(** Lock-free single-producer, multi-consumer dynamic-size double-ended queue (deque).

    The main strength of deque in a typical work-stealing setup with per-core structure is
    efficient work distribution. Owner uses [push] and [pop] method to operate at one end
    of the deque, while other (free) cores can efficiently steal work on the other side.

    This approach is great for throughput. Stealers and owner working on different sides
    reduces contention in work distribution. Further, local LIFO order runs related tasks
    one after one improves locality.

    On the other hand, the local LIFO order does not offer any fairness guarantees. Thus,
    it is not the best choice when tail latency matters. *)

open! Base

type 'a t : mutable_data with 'a @@ contended portable

(** [create ()] returns a new empty work-stealing deque. *)
val create : unit -> 'a t

(** {1 Queue owner functions}

    All these functions require the queue to be [uncontended], since they only work within
    the capsule that the queue lives in. *)

(** [push owner v] adds [v] to the front of the queue [owner]. *)
val push : 'a t -> 'a @ contended portable -> unit

val of_list : 'a list @ contended portable -> 'a t

(** [pop_exn owner] removes and returns the first element in queue [owner].

    @raise Empty if the queue is empty. *)
val pop_exn : 'a t -> 'a @ contended portable

(** [pop owner] removes and returns the first element in queue [owner], or returns [Null]
    if the queue is empty. *)
val pop : 'a t -> 'a or_null @ contended portable

(** [pop_opt owner] removes and returns the first element in queue [owner], or returns
    [None] if the queue is empty. *)
val pop_opt : 'a t -> 'a option @ contended portable

(** {1 Stealer functions}

    All these functions take the queue at the [contended] mode, since they work even if
    the queue came from another capsule. *)

(** [steal_exn stealer] removes and returns the last element from queue [stealer].

    @raise Empty if the queue is empty. *)
val steal_exn : 'a t @ contended -> 'a @ contended portable

(** [steal stealer] removes and returns the last element from queue [stealer], or returns
    [Null] if the queue is empty. *)
val steal : 'a t @ contended -> 'a or_null @ contended portable

(** [steal_opt stealer] removes and returns the last element from queue [stealer], or
    returns [None] if the queue is empty. *)
val steal_opt : 'a t @ contended -> 'a option @ contended portable

module For_testing : sig
  val blit_circularly
    :  src:'a array
    -> src_pos:int
    -> dst:'a array
    -> dst_pos:int
    -> len:int
    -> unit
end

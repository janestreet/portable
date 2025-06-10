(* Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2021, Tom Kelly <ctk21@cl.cam.ac.uk>
 * Copyright (c) 2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

(* Work Stealing Queue
 *
 * See:
 *   Dynamic circular work-stealing deque
 *   https://dl.acm.org/doi/10.1145/1073970.1073974
 *  &
 *   Correct and efficient work-stealing for weak memory models
 *   https://dl.acm.org/doi/abs/10.1145/2442516.2442524
*)

open! Base
open Modes
module Atomic = Portable_kernel.Atomic

(** This must be a power of two. *)
let min_capacity = 16

type 'a t : mutable_data with 'a @@ contended portable =
  { top : int Atomic.t
  ; bottom : int Atomic.t
  ; top_cache : int ref
  ; mutable tab : 'a Portended.t Uopt.t ref array
  }
[@@unsafe_allow_any_mode_crossing (* see below for safety *)]

module _ = struct
  [@@@disable_unused_warnings]

  module Portended = struct
    type 'a t = { portended : 'a @@ contended portable }
  end

  type 'a t : mutable_data with 'a @@ contended portable =
    { top : int Atomic.t
    ; bottom : int Atomic.t
    ; top_cache : int ref
    ; mutable tab : 'a Portended.t Uopt.t ref array
    }
end

let create () =
  let top = Atomic.make_alone 0 in
  let tab =
    Array.create
      ~len:min_capacity
      (ref (Uopt.none |> Portability_hacks.magic_uncontended__promise_deeply_immutable))
  in
  let bottom = Atomic.make_alone 0 in
  let top_cache = ref 0 |> Portable_common.Padding.copy_as_padded in
  { top; bottom; top_cache; tab } |> Portable_common.Padding.copy_as_padded
;;

let blit_circularly ~src ~src_pos ~dst ~dst_pos ~len =
  let src_len = Array.length src in
  let dst_len = Array.length dst in
  assert (
    len >= 0
    && src_pos >= 0
    && src_pos < src_len
    && len <= src_len
    && dst_pos >= 0
    && dst_pos < dst_len
    && len <= dst_len);
  let src_remaining = src_len - src_pos in
  let dst_remaining = dst_len - dst_pos in
  let first_src_chunk_len = min src_remaining len in
  let first_dst_chunk_len = min dst_remaining first_src_chunk_len in
  Array.blit ~src ~src_pos ~dst ~dst_pos ~len:first_dst_chunk_len;
  if first_dst_chunk_len < first_src_chunk_len
  then
    Array.blit
      ~src
      ~src_pos:(src_pos + first_dst_chunk_len)
      ~dst
      ~dst_pos:0
      ~len:(first_src_chunk_len - first_dst_chunk_len);
  let remaining_len = len - first_src_chunk_len in
  if remaining_len > 0
  then (
    let new_dst_pos =
      if dst_pos + first_src_chunk_len >= dst_len
      then first_src_chunk_len - first_dst_chunk_len
      else dst_pos + first_src_chunk_len
    in
    let third_dst_chunk_len = min remaining_len (dst_len - new_dst_pos) in
    Array.blit ~src ~src_pos:0 ~dst ~dst_pos:new_dst_pos ~len:third_dst_chunk_len;
    let remaining_len = remaining_len - third_dst_chunk_len in
    if remaining_len > 0
    then Array.blit ~src ~src_pos:third_dst_chunk_len ~dst ~dst_pos:0 ~len:remaining_len)
;;

let realloc a t b sz new_sz =
  let new_a =
    Array.create
      ~len:new_sz
      (ref (Uopt.none |> Portability_hacks.magic_uncontended__promise_deeply_immutable))
  in
  blit_circularly
    ~src:a
    ~src_pos:(t land (sz - 1))
    ~dst:new_a
    ~dst_pos:(t land (new_sz - 1))
    ~len:(b - t);
  new_a
;;

let push q v =
  let v = ref (Uopt.some { portended = v }) in
  (* Read of [bottom] by the owner simply does not require a fence as the
     [bottom] is only mutated by the owner. *)
  let b = Atomic.Expert.fenceless_get q.bottom in
  let t_cache = !(q.top_cache) in
  let a = q.tab in
  let size = b - t_cache in
  let capacity = Array.length a in
  if size < capacity
     ||
     let t = Atomic.get q.top in
     q.top_cache := t;
     not (phys_equal t t_cache)
  then (
    Array.unsafe_set a (b land (capacity - 1)) v;
    Atomic.incr q.bottom)
  else (
    let a = realloc a t_cache b capacity (capacity lsl 1) in
    Array.unsafe_set a (b land (Array.length a - 1)) v;
    q.tab <- a;
    Atomic.incr q.bottom)
;;

let of_list xs =
  let t = create () in
  let rec aux = function
    | [] -> ()
    | x :: xs ->
      push t x;
      aux xs
  in
  aux xs;
  t
;;

type ('a, _ : value_or_null) poly =
  | Option : ('a, 'a option) poly
  | Unboxed : ('a, 'a or_null) poly
  | Value : ('a, 'a) poly

let pop_as : type a (r : value_or_null). a t -> (a, r) poly -> r @ contended portable =
  fun q poly ->
  let b = Atomic.fetch_and_add q.bottom (-1) - 1 in
  (* Read of [top] at this point requires no fence as we simply need to ensure
     that the read happens after updating [bottom]. *)
  let t = Atomic.Expert.fenceless_get q.top in
  let size = b - t in
  if 0 < size
  then (
    let a = q.tab in
    let capacity = Array.length a in
    let out = Array.unsafe_get a (b land (capacity - 1)) in
    let res = !out in
    out := Uopt.none |> Portability_hacks.magic_uncontended__promise_deeply_immutable;
    if size + size + size <= capacity - min_capacity
    then q.tab <- realloc a t b capacity (capacity lsr 1);
    let res = Uopt.unsafe_value res in
    match poly with
    | Unboxed -> This res.portended
    | Option -> Some res.portended
    | Value -> res.portended)
  else if b = t
  then (
    (* Whether or not the [compare_and_set] below succeeds, [top_cache] can be
       updated, because in either case [top] has been incremented. *)
    q.top_cache := t + 1;
    let got = Atomic.compare_and_set q.top ~if_phys_equal_to:t ~replace_with:(t + 1) in
    Atomic.set q.bottom (b + 1);
    match got with
    | Set_here ->
      let a = q.tab in
      let out = Array.unsafe_get a (b land (Array.length a - 1)) in
      let res = !out in
      out := Uopt.none |> Portability_hacks.magic_uncontended__promise_deeply_immutable;
      let res = Uopt.unsafe_value res in
      (match poly with
       | Unboxed -> This res.portended
       | Option -> Some res.portended
       | Value -> res.portended)
    | Compare_failed ->
      (match poly with
       | Unboxed -> Null
       | Option -> None
       | Value -> failwith "Ws_deque.pop_exn called on empty deque"))
  else (
    Atomic.set q.bottom (b + 1);
    match poly with
    | Unboxed -> Null
    | Option -> None
    | Value -> failwith "Ws_deque.pop_exn called on empty deque")
;;

let pop_exn q = pop_as q Value
let pop q = pop_as q Unboxed
let pop_opt q = pop_as q Option

external magic_uncontended : 'a @ contended -> 'a @@ portable = "%identity"

let rec steal_as
  : type a (r : value_or_null).
    a t @ contended -> Backoff.t -> (a, r) poly -> r @ contended portable
  =
  fun q backoff poly ->
  (* Read of [top] does not require a fence at this point, but the read of
     [top] must happen before the read of [bottom].  The write of [top] later
     has no effect in case we happened to read an old value of [top]. *)
  let t = Atomic.Expert.fenceless_get q.top in
  let b = Atomic.get q.bottom in
  let size = b - t in
  if 0 < size
  then (
    (* Magic: we are protecting any accesses to this array behind atomic operations on
       [q.bottom] *)
    let a = (magic_uncontended q).tab in
    let out = Array.unsafe_get a (t land (Array.length a - 1)) in
    match Atomic.compare_and_set q.top ~if_phys_equal_to:t ~replace_with:(t + 1) with
    | Set_here ->
      let res = !out in
      out := Uopt.none |> Portability_hacks.magic_uncontended__promise_deeply_immutable;
      let res = Uopt.unsafe_value res in
      (match poly with
       | Unboxed -> This res.portended
       | Option -> Some res.portended
       | Value -> res.portended)
    | Compare_failed -> steal_as q (Backoff.once backoff) poly)
  else (
    match poly with
    | Unboxed -> Null
    | Option -> None
    | Value -> failwith "Ws_deque.steal_exn called on empty deque")
;;

let steal_exn q = steal_as q Backoff.default Value
let steal q = steal_as q Backoff.default Unboxed
let steal_opt q = steal_as q Backoff.default Option

module For_testing = struct
  let blit_circularly = blit_circularly
end

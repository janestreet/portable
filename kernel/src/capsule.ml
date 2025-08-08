open! Base
include Capsule_intf.Definitions
module Expert = Basement.Capsule

module Access = struct
  type 'k t = 'k Expert.Access.t
  type packed = Expert.Access.packed = P : 'k t -> packed [@@unboxed]

  let current = Expert.current
end

module Data = struct
  type ('a, 'k) t = ('a, 'k) Expert.Data.t

  let create = Expert.Data.create
  let wrap = Expert.Data.wrap
  let unwrap = Expert.Data.unwrap
  let return = Expert.Data.inject
  let get_id = Expert.Data.project
  let get_id_contended = Expert.Data.project
  let both = Expert.Data.both
  let fst = Expert.Data.fst
  let snd = Expert.Data.snd

  [%%template
  [@@@mode.default local]

  let wrap = Expert.Data.Local.wrap
  let unwrap = Expert.Data.Local.unwrap
  let return = Expert.Data.Local.inject
  let get_id = Expert.Data.Local.project
  let get_id_contended = Expert.Data.Local.project
  let both = Expert.Data.Local.both
  let fst = Expert.Data.Local.fst
  let snd = Expert.Data.Local.snd]
end

module Initial = struct
  type k = Expert.initial

  let access = Expert.initial

  let%template with_access_opt ~f =
    (Basement.Stdlib_shim.Domain.Safe.DLS.access [@mode l]) (fun access ->
      f (Expert.get_initial access) [@exclave_if_stack a] [@nontail])
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  module Data = struct
    type 'a t = ('a, k) Data.t

    [%%template
    [@@@mode.default l = (global, local)]

    let wrap a = (Data.wrap [@mode l]) ~access:Expert.initial a [@exclave_if_local l]
    let unwrap a = (Data.unwrap [@mode l]) ~access:Expert.initial a [@exclave_if_local l]]

    [%%template
    [@@@alloc.default a @ l = (heap_global, stack_local)]

    let[@inline] get_opt a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match[@exclave_if_stack a] access with
        | Some access -> Some (f ((Data.unwrap [@mode l]) ~access a))
        | None -> None)
      [@exclave_if_stack a] [@nontail]
    ;;

    (* NOTE: This isn't defined in terms of [get_opt] to avoid allocating the extra
       option *)
    let if_on_initial a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match access with
        | Some access -> f ((Data.unwrap [@mode l]) ~access a) [@nontail]
        | None -> ())
      [@nontail]
    ;;

    let get_exn a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match[@exclave_if_stack a] access with
        | Some access -> f ((Data.unwrap [@mode l]) ~access a) [@nontail]
        | None ->
          failwith
            "[Capsule.Initial.Data.get_exn] called from a capsule other than the initial \
             capsule.")
      [@exclave_if_stack a] [@nontail]
    ;;

    let iter_exn a ~f = (get_exn [@alloc a]) a ~f:(fun b : unit -> f b) [@nontail]]

    let sexp_of_t sexp_of_a t = sexp_of_a (unwrap t)
    let t_of_sexp a_of_sexp a = wrap (a_of_sexp a)
  end
end

module Mutex = struct
  type 'k t = 'k Expert.Mutex.t
  type packed = Expert.Mutex.packed = P : 'k t -> packed [@@unboxed]

  let create () =
    let (P (type k) (key : k Expert.Key.t)) = Expert.create () in
    P (Expert.Mutex.create key)
  ;;

  let create_m () : (module Module_with_mutex) =
    let (P (type k) (t : k t)) = create () in
    (module struct
      type nonrec k = k

      let mutex = t
    end)
  ;;

  module Create () = (val create_m ())

  let[@inline] with_lock t ~f =
    (Expert.Mutex.with_lock t ~f:(fun password ->
       { portended =
           Expert.access ~password ~f:(fun access -> { many = { aliased = f access } })
       }))
      .portended
      .many
      .aliased
  ;;
end

module Isolated = struct
  type ('a, 'k) inner =
    { key : 'k Expert.Key.t
    ; data : ('a, 'k) Data.t
    }

  type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let create f =
    let (P key) = Expert.create () in
    let data = Data.create f in
    P { key; data }
  ;;

  let with_unique_gen (P { key; data }) ~f =
    let result, key =
      Expert.Key.access key ~f:(fun access ->
        { many = f (Expert.Data.unwrap ~access data) })
    in
    P { key; data }, result.many
  ;;

  let with_unique t ~f = with_unique_gen t ~f:(fun x -> { aliased = f x }) [@nontail]

  let with_shared_gen (P { key; data }) ~f =
    (Expert.Key.access_shared key ~f:(fun access ->
       { aliased = { many = f (Expert.Data.unwrap_shared ~access data) } })
    [@nontail])
      .aliased
      .many
  ;;

  let with_shared = with_shared_gen

  let unwrap (P { key; data }) =
    let access = Expert.Key.destroy key in
    Expert.Data.unwrap ~access data
  ;;

  let%template[@mode local] unwrap (P { key; data }) =
    let access = Expert.Key.destroy key in
    Expert.Data.Local.unwrap ~access data
  ;;

  let get_id_contended (P { data; key }) =
    P { data; key }, { aliased = Data.get_id_contended data }
  ;;
end

module Guard = struct
  type ('a, 'k) inner =
    { data : ('a, 'k) Data.t
    ; password : 'k Expert.Password.t
    }

  type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let[@inline] with_ a ~f =
    let (P access) = Access.current () in
    let data = Data.wrap ~access a in
    Expert.Password.with_current access (fun password ->
      f (P { data; password }) [@nontail])
    [@nontail]
  ;;

  let[@inline] get (P { data; password }) ~f =
    (Expert.Data.extract data ~password ~f:(fun a -> { many = { aliased = f a } })).many
      .aliased
  ;;

  let get_contended = get
  let[@inline] iter (P { data; password }) ~f = Expert.Data.iter data ~password ~f

  let[@inline] map (P { data; password }) ~f =
    P { data = Expert.Data.map data ~password ~f; password }
  ;;
end

module With_mutex = struct
  type ('a, 'k) inner =
    { data : ('a, 'k) Data.t
    ; mutex : 'k Mutex.t
    }

  type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let create f =
    let (P mutex) = Mutex.create () in
    let data = Data.create f in
    P { data; mutex }
  ;;

  let of_isolated (Isolated.P { key; data }) =
    let mutex = Expert.Mutex.create key in
    P { mutex; data }
  ;;

  let with_lock (P { mutex; data }) ~f =
    Mutex.with_lock mutex ~f:(fun access -> f (Data.unwrap ~access data)) [@nontail]
  ;;

  let iter = with_lock

  let map (P { mutex; data }) ~f =
    let data =
      Mutex.with_lock mutex ~f:(fun access ->
        Data.wrap ~access (f (Data.unwrap ~access data)))
    in
    P { mutex; data }
  ;;

  let destroy (P { mutex; data }) =
    let key = Expert.Mutex.destroy mutex in
    let access = Expert.Key.destroy key in
    Expert.Data.unwrap data ~access
  ;;
end

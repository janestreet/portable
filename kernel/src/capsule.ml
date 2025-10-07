open! Base
module Expert = Basement.Capsule

module Access = struct
  type 'k t = 'k Expert.Access.t
  type packed = Expert.Access.packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed = 'k Expert.Access.boxed

  let current = Expert.current
  let unbox = Expert.Access.unbox
  let box = Expert.Access.box
end

module Data = struct
  type ('a, 'k) t : value mod contended portable = ('a, 'k) Expert.Data.t

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

  let with_access_opt ~f =
    (Expert.access_initial (fun access -> { global = { aliased = f access } })).global
      .aliased
  ;;

  let%template with_access_opt ~f = exclave_
    (Expert.access_initial (fun access -> exclave_ { aliased = f access })).aliased
  [@@alloc a @ l = stack_local]
  ;;

  module Data = struct
    type 'a t = ('a, k) Data.t

    [%%template
    [@@@mode.default l = (global, local)]

    let wrap a =
      let access = Access.unbox access in
      (Data.wrap [@mode l]) ~access a [@exclave_if_local l]
    ;;

    let unwrap a =
      let access = Access.unbox access in
      (Data.unwrap [@mode l]) ~access a [@exclave_if_local l]
    ;;]

    [%%template
    [@@@alloc.default a @ l = (heap_global, stack_local)]

    let[@inline] get_opt a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match[@exclave_if_stack a] access with
        | Some access ->
          Some (f ((Data.unwrap [@mode l]) ~access:(Access.unbox access) a))
        | None -> None)
      [@exclave_if_stack a] [@nontail]
    ;;

    (* NOTE: This isn't defined in terms of [get_opt] to avoid allocating the extra
       option *)
    let if_on_initial a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match access with
        | Some access ->
          f ((Data.unwrap [@mode l]) ~access:(Access.unbox access) a) [@nontail]
        | None -> ())
      [@nontail]
    ;;

    let get_exn a ~f =
      (with_access_opt [@alloc a]) ~f:(fun access ->
        match[@exclave_if_stack a] access with
        | Some access ->
          f ((Data.unwrap [@mode l]) ~access:(Access.unbox access) a) [@nontail]
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

module Isolated = struct
  type ('a, 'k) inner : value mod contended portable =
    { key : 'k Expert.Key.t @@ global
    ; data : ('a, 'k) Data.t @@ aliased
    }

  type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let create f =
    let (P key) = Expert.create () in
    let data = Data.create f in
    P { key; data }
  ;;

  let with_unique_gen (P { key; data }) ~f =
    let #(result, key) =
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
  let unwrap_shared (P { key; data }) = Expert.Data.project_shared ~key data

  let%template[@mode local] unwrap_shared (P { key; data }) = exclave_
    Expert.Data.Local.project_shared ~key data
  ;;

  let unwrap (P { key; data }) =
    let access = Expert.Key.destroy key in
    Expert.Data.unwrap ~access data
  ;;

  let%template[@mode local] unwrap (P { key; data }) =
    let access = Expert.Key.destroy key in
    exclave_ Expert.Data.Local.unwrap ~access data
  ;;

  let get_id_contended (P { data; key }) =
    P { data; key }, { aliased = Data.get_id_contended data }
  ;;
end

module Guard = struct
  type ('a, 'k) inner : value mod contended portable =
    { data : ('a, 'k) Data.t @@ global
    ; password : 'k Expert.Password.t
    }

  type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let[@inline] with_ a ~f =
    let (P access) = Access.current () in
    let data = Data.wrap ~access a in
    (Expert.Password.with_current access (fun password -> exclave_
       { aliased = { global = f (P { data; password }) } }))
      .aliased
      .global
  ;;

  let[@inline] get (P { data; password }) ~f =
    (Expert.Data.extract data ~password ~f:(fun a -> { many = { aliased = f a } })).many
      .aliased
  ;;

  let get_contended = get
  let iter = get

  let[@inline] map (P { data; password }) ~f = exclave_
    P { data = Expert.Data.map data ~password ~f; password }
  ;;
end

module Shared = struct
  type ('a, 'k) inner : value mod contended portable =
    { data : ('a shared, 'k) Data.t @@ global
    ; password : 'k Expert.Password.Shared.t
    }

  type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

  module Uncontended = struct
    type ('a, 'k) t : value mod contended portable = ('a, 'k) inner =
      { data : ('a shared, 'k) Data.t @@ global
      ; password : 'k Expert.Password.Shared.t
      }

    type ('a, 'b) f =
      { f : 'k. ('a, 'k) inner @ local unyielding -> ('b, 'k) Expert.Data.Shared.t }

    let[@inline] with_ data { f } =
      let (P access) = Access.current () in
      let data = Data.wrap ~access { shared = data } in
      let { global = { aliased = data } } =
        Expert.Password.with_current access (fun [@inline] password ->
          let password = Expert.Password.shared password in
          Expert.Password.Shared.borrow password (fun [@inline] password ->
            { global = { aliased = f { data; password } } })
          [@nontail])
      in
      Expert.Data.Shared.unwrap ~access data
    ;;

    let[@inline] get { data; password } ~f =
      Expert.Data.Shared.map_into data ~password ~f:(fun [@inline] { shared } -> f shared)
      [@nontail]
    ;;

    let[@inline] map { data; password } ~f = exclave_
      { data =
          Expert.Data.map_shared data ~password ~f:(fun [@inline] { shared } ->
            { shared = f shared })
      ; password
      }
    ;;
  end

  let[@inline] with_ data ~f =
    let (P access) = Access.current () in
    let data = Data.wrap ~access { shared = data } in
    (Expert.Password.with_current access (fun [@inline] password ->
       let password = Expert.Password.shared password in
       Expert.Password.Shared.borrow password (fun [@inline] password ->
         { global = { aliased = f (P { data; password }) } })
       [@nontail]))
      .global
      .aliased
  ;;

  let[@inline] get (P t) ~f = Expert.Data.Shared.project (Uncontended.get t ~f)

  let[@inline] get_contended t ~f =
    (get t ~f:(fun [@inline] a -> { portended = f a })).portended
  ;;

  let iter = get
  let[@inline] map (P t) ~f = exclave_ P (Uncontended.map t ~f)
end

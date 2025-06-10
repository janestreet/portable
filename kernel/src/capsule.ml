open! Base
include Capsule_intf.Definitions
module Expert = Basement.Capsule

module Password = struct
  type 'k t : value mod contended portable = 'k Expert.Password.t
end

module Data = struct
  type ('a, 'k) t : value mod contended portable = ('a, 'k) Expert.Data.t

  let create = Expert.Data.create
  let return = Expert.Data.inject
  let get_id = Expert.Data.project
  let get_id_contended = Expert.Data.project
  let both = Expert.Data.both
  let fst = Expert.Data.fst
  let snd = Expert.Data.snd
  let[@inline] map t ~f ~password = Expert.Data.map t ~password ~f

  let[@inline] get t ~f ~password =
    (Expert.Data.extract t ~password ~f:(fun a -> { many = { aliased = f a } })).many
      .aliased
  ;;

  let[@inline] get_contended t ~f ~password =
    (Expert.Data.extract t ~password ~f:(fun a -> { many = { aliased = f a } })).many
      .aliased
  ;;

  let[@inline] bind t ~f ~password = Expert.Data.bind t ~password ~f
  let[@inline] iter t ~f ~password = Expert.Data.iter t ~password ~f
end

module Initial = struct
  type k = Expert.initial

  module Data = struct
    type 'a t = ('a, k) Data.t

    let wrap a = Expert.Data.wrap ~access:Expert.initial a
    let unwrap a = Expert.Data.unwrap ~access:Expert.initial a

    [%%template
    [@@@mode.default local]

    let wrap a = exclave_ Expert.Data.Local.wrap ~access:Expert.initial a
    let unwrap a = exclave_ Expert.Data.Local.unwrap ~access:Expert.initial a]

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
    (Expert.Mutex.with_lock t ~f:(fun a -> { many = { aliased = f a } })).many.aliased
  ;;
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
    exclave_ Expert.Data.Local.unwrap ~access data
  ;;

  let get_id_contended (P { data; key }) =
    P { data; key }, { aliased = Data.get_id_contended data }
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

  module Guard = struct
    type ('a, 'k) inner =
      { password : 'k Password.t
      ; data : ('a, 'k) Data.t @@ global
      }

    (** A value of this type represents a currently-locked [Capsule.With_mutex.t] *)
    type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]
  end

  let with_lock (P { mutex; data }) ~f =
    Mutex.with_lock mutex ~f:(fun password -> f (Guard.P { password; data }) [@nontail])
    [@nontail]
  ;;

  let get (P { mutex; data }) ~f =
    (Mutex.with_lock mutex ~f:(fun password ->
       Data.get ~password data ~f:(fun x -> { portable = f x }) |> Modes.Contended.cross))
      .portable
  ;;

  let map (P { mutex; data }) ~f =
    let data = Mutex.with_lock mutex ~f:(fun password -> Data.map ~password ~f data) in
    P { mutex; data }
  ;;

  let destroy (P { mutex; data }) =
    let key = Expert.Mutex.destroy mutex in
    let access = Expert.Key.destroy key in
    Expert.Data.unwrap data ~access
  ;;
end

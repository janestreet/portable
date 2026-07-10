open! Core
open Portable_kernel
open Expect_test_helpers_core

(* Some examples of using the API for [Capsule] exposed in [Portable]. *)

module%test [@name "[Capsule.Owned] and [Capsule.Frozen]"] _ = struct
  module Some_library = struct
    let do_stuff (r : int ref) : string =
      r := 1;
      "hello"
    ;;
  end

  let%expect_test _ =
    let data = Capsule.Owned.create (fun () -> { aliased = ref 0 }) in
    let #(data, do_stuff_result) =
      Capsule.Owned.with_ data ~f:(fun r -> Some_library.do_stuff r.aliased)
    in
    (* Even though [get] is [portable], it can still read the contents of [data] since it
       has [shared] access to it. *)
    let data = Capsule.Owned.freeze data in
    let (get @ portable) () =
      let r = Capsule.Frozen.unwrap data in
      r.aliased.contents
    in
    print_s [%message (get () : int)];
    print_s [%message (do_stuff_result : string)];
    [%expect
      {|
      ("get ()" 1)
      (do_stuff_result hello)
      |}]
  ;;
end

[@@@alert "-maybe_still_concurrent"]

[%%test
  module _ = struct
    [%%expect_test
      let "[is_initial_*] allocation" =
        ignore (require_no_allocation Capsule.Prim.is_initial_domain : bool);
        ignore (require_no_allocation Capsule.Prim.is_initial_thread : bool)
      ;;]
  end
  [@@name "initial capsule"]]

module%test [@name "[Capsule.Shared]"] _ = struct
  let fork_join
    :  (unit -> 'a) @ local once -> (unit -> 'b) @ local once portable unyielding
    -> 'a * 'b
    =
    fun f g ->
    let a = f () in
    let b = g () in
    a, b
  ;;

  let%expect_test "crossing" =
    let array = [| "foo"; "bar" |] in
    let result =
      Capsule.Scoped.Shared.with_ array ~f:(fun shared ->
        let a, b =
          fork_join
            (fun () ->
              Capsule.Scoped.Shared.get shared ~f:(fun array ->
                (Array.get [@mode shared]) array 0))
            (fun () ->
              Capsule.Scoped.Shared.get shared ~f:(fun array ->
                (Array.get [@mode shared]) array 1))
        in
        a ^ b)
    in
    print_s [%message (result : string)];
    [%expect {| (result foobar) |}]
  ;;

  let%expect_test "uncontended" =
    let array = [| "foo"; "bar" |] in
    let result =
      Capsule.Scoped.Shared.Uncontended.with_
        array
        { f =
            (fun shared ->
              let a, b =
                fork_join
                  (fun () ->
                    Capsule.Scoped.Shared.Uncontended.get shared ~f:(fun array ->
                      ref ((Array.get [@mode shared]) array 0)))
                  (fun () ->
                    Capsule.Scoped.Shared.Uncontended.get shared ~f:(fun array ->
                      ref ((Array.get [@mode shared]) array 1)))
              in
              Capsule.Prim.Data.Shared.both a b)
        }
    in
    print_s [%message (result : string ref * string ref)];
    [%expect {| (result (foo bar)) |}]
  ;;

  let%expect_test "with_ doesn't allocate" =
    let x = [| "foo"; "bar" |] in
    ignore
      (require_no_allocation (fun () ->
         Capsule.Scoped.Shared.with_ x ~f:(fun g ->
           Capsule.Scoped.Shared.get g ~f:(fun s -> (Array.get [@mode shared]) s 0)))
       : string)
  ;;

  let%expect_test "Uncontended.with_ doesn't allocate" =
    let x = [| "foo"; "bar" |] in
    ignore
      (require_no_allocation (fun () ->
         Capsule.Scoped.Shared.Uncontended.with_
           x
           { f =
               (fun g ->
                 Capsule.Scoped.Shared.Uncontended.get g ~f:(fun (s : string array) ->
                   (Array.get [@mode shared]) s 0))
           })
       : string)
  ;;
end

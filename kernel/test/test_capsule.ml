open! Core
open Portable_kernel
open Expect_test_helpers_core

(* Some examples of using the API for [Capsule] exposed in [Portable]. *)

module%test [@name "[Capsule.Isolated]"] _ = struct
  module Some_library = struct
    let do_stuff (r : int ref) : string =
      r := 1;
      "hello"
    ;;
  end

  let%expect_test _ =
    let data = Capsule.Isolated.create (fun () -> ref 0) in
    let data, { aliased = do_stuff_result } =
      Capsule.Isolated.with_unique data ~f:(fun r -> Some_library.do_stuff r)
    in
    (* Even though [get] is [portable], it can still read the contents of [data] since it
     has [shared] access to it. *)
    let (get @ portable) () =
      Capsule.Isolated.with_shared data ~f:(fun r -> r.contents)
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

module%test [@name "[Capsule.Initial]"] _ = struct
  let%expect_test "[if_on_initial] allocation" =
    let capsule = Capsule.Initial.Data.wrap "foo" in
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.if_on_initial [@alloc stack])
        capsule
        ~f:(ignore : string @ local -> unit));
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.if_on_initial [@alloc heap])
        capsule
        ~f:(ignore : string @ local -> unit))
  ;;

  let%expect_test "[iter_exn] allocation in the happy case" =
    let capsule = Capsule.Initial.Data.wrap [| "foo" |] in
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.iter_exn [@alloc stack])
        capsule
        ~f:(ignore : string array @ local -> unit));
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.iter_exn [@alloc heap])
        capsule
        ~f:(ignore : string array @ local -> unit))
  ;;
end

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
      Capsule.Shared.with_ array ~f:(fun shared ->
        let a, b =
          fork_join
            (fun () ->
              Capsule.Shared.get shared ~f:(fun array ->
                (Array.get [@mode shared]) array 0))
            (fun () ->
              Capsule.Shared.get shared ~f:(fun array ->
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
      Capsule.Shared.Uncontended.with_
        array
        { f =
            (fun shared ->
              let a, b =
                fork_join
                  (fun () ->
                    Capsule.Shared.Uncontended.get shared ~f:(fun array ->
                      ref ((Array.get [@mode shared]) array 0)))
                  (fun () ->
                    Capsule.Shared.Uncontended.get shared ~f:(fun array ->
                      ref ((Array.get [@mode shared]) array 1)))
              in
              Capsule.Expert.Data.Shared.both a b)
        }
    in
    print_s [%message (result : string ref * string ref)];
    [%expect {| (result (foo bar)) |}]
  ;;

  let%expect_test "with_ doesn't allocate" =
    let x = [| "foo"; "bar" |] in
    ignore
      (require_no_allocation (fun () ->
         Capsule.Shared.with_ x ~f:(fun g ->
           Capsule.Shared.get g ~f:(fun s -> (Array.get [@mode shared]) s 0)))
       : string)
  ;;

  let%expect_test "Uncontended.with_ doesn't allocate" =
    let x = [| "foo"; "bar" |] in
    ignore
      (require_no_allocation (fun () ->
         Capsule.Shared.Uncontended.with_
           x
           { f =
               (fun g ->
                 Capsule.Shared.Uncontended.get g ~f:(fun (s : string array) ->
                   (Array.get [@mode shared]) s 0))
           })
       : string)
  ;;
end

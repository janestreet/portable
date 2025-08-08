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
    let get () = Capsule.Isolated.with_shared data ~f:(fun r -> r.contents) in
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
        ~f:(ignore : string -> unit));
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.if_on_initial [@alloc heap])
        capsule
        ~f:(ignore : string -> unit))
  ;;

  let%expect_test "[iter_exn] allocation in the happy case" =
    let capsule = Capsule.Initial.Data.wrap [| "foo" |] in
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.iter_exn [@alloc stack])
        capsule
        ~f:(ignore : string array -> unit));
    require_no_allocation (fun () ->
      (Capsule.Initial.Data.iter_exn [@alloc heap])
        capsule
        ~f:(ignore : string array -> unit))
  ;;
end

open! Core
open! Portable_kernel
open Expect_test_helpers_core

let%expect_test "compare_and_set" =
  let atomic = Atomic.make 1 in
  let compare_failed =
    Atomic.compare_and_set atomic ~if_phys_equal_to:4 ~replace_with:10
  in
  let current_value = Atomic.get atomic in
  print_s
    [%message
      (compare_failed : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
  [%expect
    {|
    ((compare_failed Compare_failed)
     (current_value  1))
    |}];
  let set_here = Atomic.compare_and_set atomic ~if_phys_equal_to:1 ~replace_with:10 in
  let current_value = Atomic.get atomic in
  print_s
    [%message (set_here : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
  [%expect
    {|
    ((set_here      Set_here)
     (current_value 10))
    |}]
;;

let%expect_test "update_and_return" =
  let atomic = Atomic.make 1 in
  let result = Atomic.update_and_return atomic ~pure_f:(fun x -> x + 1) in
  let new_value = Atomic.get atomic in
  print_s [%message (result : int) (new_value : int)];
  [%expect
    {|
    ((result    1)
     (new_value 2))
    |}]
;;

let%expect_test "update doesn't allocate" =
  let atomic = Atomic.make 1 in
  require_no_allocation (fun () -> Atomic.update atomic ~pure_f:(fun x -> x + 1))
;;

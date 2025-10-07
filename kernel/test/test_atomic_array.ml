open! Base
open Portable_kernel
open Atomic_array
open Expect_test_helpers_base
open Base_quickcheck.Export

let%expect_test "basic operations" =
  let arr = create ~len:3 42 in
  print_s [%message (length arr : int)];
  [%expect {| ("length arr" 3) |}];
  print_s [%message (get arr 0 : int)];
  [%expect {| ("get arr 0" 42) |}];
  print_s [%message (get arr 1 : int)];
  [%expect {| ("get arr 1" 42) |}];
  print_s [%message (get arr 2 : int)];
  [%expect {| ("get arr 2" 42) |}]
;;

let%expect_test "set and exchange" =
  let arr = create ~len:2 10 in
  set arr 0 20;
  print_s [%message "after set" ~value:(get arr 0 : int)];
  [%expect {| ("after set" (value 20)) |}];
  let old = exchange arr 1 30 in
  print_s [%message "exchange" ~old:(old : int) ~new_val:(get arr 1 : int)];
  [%expect
    {|
    (exchange
      (old     10)
      (new_val 30))
    |}]
;;

let%expect_test "integer operations" =
  let arr = create ~len:1 100 in
  let old = fetch_and_add arr 0 5 in
  print_s [%message "fetch_and_add 5" ~old:(old : int) ~new_val:(get arr 0 : int)];
  [%expect
    {|
    ("fetch_and_add 5"
      (old     100)
      (new_val 105))
    |}];
  add arr 0 10;
  print_s [%message "after add 10" (get arr 0 : int)];
  [%expect {| ("after add 10" ("get arr 0" 115)) |}];
  sub arr 0 3;
  print_s [%message "after sub 3" (get arr 0 : int)];
  [%expect {| ("after sub 3" ("get arr 0" 112)) |}];
  incr arr 0;
  print_s [%message "after incr" (get arr 0 : int)];
  [%expect {| ("after incr" ("get arr 0" 113)) |}];
  decr arr 0;
  print_s [%message "after decr" (get arr 0 : int)];
  [%expect {| ("after decr" ("get arr 0" 112)) |}]
;;

let%expect_test "bitwise operations" =
  let arr = create ~len:1 0b1111 in
  logand arr 0 0b1010;
  print_s [%message "after logand with 1010" ~result:(get arr 0 : Int.Binary.t)];
  [%expect {| ("after logand with 1010" (result 0b1010)) |}];
  logor arr 0 0b0100;
  print_s [%message "after logor with 0100" ~result:(get arr 0 : Int.Binary.t)];
  [%expect {| ("after logor with 0100" (result 0b1110)) |}];
  logxor arr 0 0b0011;
  print_s [%message "after logxor with 0011" ~result:(get arr 0 : Int.Binary.t)];
  [%expect {| ("after logxor with 0011" (result 0b1101)) |}]
;;

let%expect_test "compare_and_set" =
  let arr = create ~len:1 50 in
  let result1 = compare_and_set arr 0 ~if_phys_equal_to:50 ~replace_with:60 in
  print_s
    [%message
      "cas success" (result1 : Compare_failed_or_set_here.t) ~value:(get arr 0 : int)];
  [%expect
    {|
    ("cas success"
      (result1 Set_here)
      (value   60))
    |}];
  let result2 = compare_and_set arr 0 ~if_phys_equal_to:50 ~replace_with:70 in
  print_s
    [%message
      "cas failure" (result2 : Compare_failed_or_set_here.t) ~value:(get arr 0 : int)];
  [%expect
    {|
    ("cas failure"
      (result2 Compare_failed)
      (value   60))
    |}]
;;

let%expect_test "compare_exchange" =
  let arr = create ~len:1 80 in
  let old1 = compare_exchange arr 0 ~if_phys_equal_to:80 ~replace_with:90 in
  print_s [%message "cmpxchg success" ~old:(old1 : int) ~new_val:(get arr 0 : int)];
  [%expect
    {|
    ("cmpxchg success"
      (old     80)
      (new_val 90))
    |}];
  let old2 = compare_exchange arr 0 ~if_phys_equal_to:80 ~replace_with:100 in
  print_s [%message "cmpxchg failure" ~old:(old2 : int) ~new_val:(get arr 0 : int)];
  [%expect
    {|
    ("cmpxchg failure"
      (old     90)
      (new_val 90))
    |}]
;;

let%expect_test "bounds checking" =
  let arr = create ~len:2 0 in
  require_does_raise (fun () -> ignore (get arr (-1) : int));
  [%expect {| (Invalid_argument "Atomic_array.get: index out of bounds") |}];
  require_does_raise (fun () -> ignore (get arr 2 : int));
  [%expect {| (Invalid_argument "Atomic_array.get: index out of bounds") |}];
  require_does_raise (fun () -> set arr (-1) 42);
  [%expect {| (Invalid_argument "Atomic_array.set: index out of bounds") |}];
  require_does_raise (fun () -> set arr 3 42);
  [%expect {| (Invalid_argument "Atomic_array.set: index out of bounds") |}];
  require_does_raise (fun () -> ignore (exchange arr (-1) 42 : int));
  [%expect {| (Invalid_argument "Atomic_array.exchange: index out of bounds") |}];
  require_does_raise (fun () -> ignore (exchange arr 2 42 : int));
  [%expect {| (Invalid_argument "Atomic_array.exchange: index out of bounds") |}];
  require_does_raise (fun () ->
    ignore
      (compare_and_set arr (-1) ~if_phys_equal_to:0 ~replace_with:1
       : Compare_failed_or_set_here.t));
  [%expect {| (Invalid_argument "Atomic_array.compare_and_set: index out of bounds") |}];
  require_does_raise (fun () ->
    ignore
      (compare_and_set arr 2 ~if_phys_equal_to:0 ~replace_with:1
       : Compare_failed_or_set_here.t));
  [%expect {| (Invalid_argument "Atomic_array.compare_and_set: index out of bounds") |}];
  require_does_raise (fun () ->
    ignore (compare_exchange arr (-1) ~if_phys_equal_to:0 ~replace_with:1 : int));
  [%expect {| (Invalid_argument "Atomic_array.compare_exchange: index out of bounds") |}];
  require_does_raise (fun () ->
    ignore (compare_exchange arr 2 ~if_phys_equal_to:0 ~replace_with:1 : int));
  [%expect {| (Invalid_argument "Atomic_array.compare_exchange: index out of bounds") |}];
  require_does_raise (fun () -> ignore (fetch_and_add arr (-1) 5 : int));
  [%expect {| (Invalid_argument "Atomic_array.fetch_and_add: index out of bounds") |}];
  require_does_raise (fun () -> ignore (fetch_and_add arr 2 5 : int));
  [%expect {| (Invalid_argument "Atomic_array.fetch_and_add: index out of bounds") |}];
  require_does_raise (fun () -> add arr (-1) 5);
  [%expect {| (Invalid_argument "Atomic_array.add: index out of bounds") |}];
  require_does_raise (fun () -> add arr 2 5);
  [%expect {| (Invalid_argument "Atomic_array.add: index out of bounds") |}];
  require_does_raise (fun () -> sub arr (-1) 3);
  [%expect {| (Invalid_argument "Atomic_array.sub: index out of bounds") |}];
  require_does_raise (fun () -> sub arr 2 3);
  [%expect {| (Invalid_argument "Atomic_array.sub: index out of bounds") |}];
  require_does_raise (fun () -> logand arr (-1) 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logand: index out of bounds") |}];
  require_does_raise (fun () -> logand arr 2 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logand: index out of bounds") |}];
  require_does_raise (fun () -> logor arr (-1) 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logor: index out of bounds") |}];
  require_does_raise (fun () -> logor arr 2 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logor: index out of bounds") |}];
  require_does_raise (fun () -> logxor arr (-1) 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logxor: index out of bounds") |}];
  require_does_raise (fun () -> logxor arr 2 0xFF);
  [%expect {| (Invalid_argument "Atomic_array.logxor: index out of bounds") |}];
  require_does_raise (fun () -> incr arr (-1));
  [%expect {| (Invalid_argument "Atomic_array.add: index out of bounds") |}];
  require_does_raise (fun () -> incr arr 2);
  [%expect {| (Invalid_argument "Atomic_array.add: index out of bounds") |}];
  require_does_raise (fun () -> decr arr (-1));
  [%expect {| (Invalid_argument "Atomic_array.sub: index out of bounds") |}];
  require_does_raise (fun () -> decr arr 2);
  [%expect {| (Invalid_argument "Atomic_array.sub: index out of bounds") |}]
;;

let%expect_test "[init]" =
  let arr = init 4 ~f:(fun i -> i * 10) in
  let values = List.init 4 ~f:(fun i -> get arr i) in
  print_s [%sexp (values : int list)];
  [%expect {| (0 10 20 30) |}]
;;

let%quick_test "get/set roundtrip" =
  fun (arr : int t) (values : int list) ->
  let len = length arr in
  let values = List.take values len in
  List.iteri values ~f:(fun i value -> set arr i value);
  List.iteri values ~f:(fun i expected -> require_equal (module Int) (get arr i) expected)
;;

let%quick_test "exchange returns old value" =
  fun (arr : int t) (new_val : int) ->
  if length arr = 0
  then ()
  else (
    let index = 0 in
    let old_val = get arr index in
    let returned = exchange arr index new_val in
    require_equal (module Int) returned old_val;
    require_equal (module Int) (get arr index) new_val)
;;

(* Integers that won't overflow on 32-bit platforms, such as WASM *)
let gen_int = Base_quickcheck.Generator.int_inclusive (-1000000) 1000000

let%quick_test "add operation" =
  fun (initial : (int[@generator gen_int])) (increment : (int[@generator gen_int])) ->
  let arr = create ~len:1 initial in
  add arr 0 increment;
  require_equal (module Int) (get arr 0) (initial + increment)
;;

let%quick_test "sub operation" =
  fun (initial : (int[@generator gen_int])) (decrement : (int[@generator gen_int])) ->
  let arr = create ~len:1 initial in
  sub arr 0 decrement;
  require_equal (module Int) (get arr 0) (initial - decrement)
;;

let%quick_test "fetch_and_add returns old value and updates" =
  fun (initial : (int[@generator gen_int])) (increment : (int[@generator gen_int])) ->
  let arr = create ~len:1 initial in
  let old = fetch_and_add arr 0 increment in
  require_equal (module Int) old initial;
  require_equal (module Int) (get arr 0) (initial + increment)
;;

let%quick_test "incr/decr operations" =
  fun (initial : (int[@generator gen_int])) ->
  let arr = create ~len:1 initial in
  incr arr 0;
  let after_incr = get arr 0 in
  decr arr 0;
  let after_decr = get arr 0 in
  require_equal (module Int) after_incr (initial + 1);
  require_equal (module Int) after_decr initial
;;

let%quick_test "bitwise and operation" =
  fun (a : int) (b : int) ->
  let arr = create ~len:1 a in
  logand arr 0 b;
  require_equal (module Int) (get arr 0) (a land b)
;;

let%quick_test "bitwise or operation" =
  fun (a : int) (b : int) ->
  let arr = create ~len:1 a in
  logor arr 0 b;
  require_equal (module Int) (get arr 0) (a lor b)
;;

let%quick_test "bitwise xor operation" =
  fun (a : int) (b : int) ->
  let arr = create ~len:1 a in
  logxor arr 0 b;
  require_equal (module Int) (get arr 0) (a lxor b)
;;

let%quick_test "compare_and_set success" =
  fun (initial : int) (new_val : int) ->
  let arr = create ~len:1 initial in
  let result = compare_and_set arr 0 ~if_phys_equal_to:initial ~replace_with:new_val in
  match result with
  | Set_here -> require_equal (module Int) (get arr 0) new_val
  | Compare_failed -> require false
;;

let%quick_test "compare_and_set failure" =
  fun (initial : int) (wrong_expected : int) (new_val : int) ->
  (* Ensure wrong_expected is actually wrong *)
  if Int.equal initial wrong_expected
  then ()
  else (
    let arr = create ~len:1 initial in
    let result =
      compare_and_set arr 0 ~if_phys_equal_to:wrong_expected ~replace_with:new_val
    in
    match result with
    | Compare_failed ->
      require_equal (module Int) (get arr 0) initial (* Value unchanged *)
    | Set_here -> require false)
;;

let%quick_test "compare_exchange success" =
  fun (initial : int) (new_val : int) ->
  let arr = create ~len:1 initial in
  let old = compare_exchange arr 0 ~if_phys_equal_to:initial ~replace_with:new_val in
  require_equal (module Int) old initial;
  require_equal (module Int) (get arr 0) new_val
;;

let%quick_test "compare_exchange failure" =
  fun (initial : int) (wrong_expected : int) (new_val : int) ->
  if Int.equal initial wrong_expected
  then ()
  else (
    let arr = create ~len:1 initial in
    let old =
      compare_exchange arr 0 ~if_phys_equal_to:wrong_expected ~replace_with:new_val
    in
    require_equal (module Int) old initial;
    require_equal (module Int) (get arr 0) initial)
;;

let%quick_test "init creates correct values" =
  fun (values : int list) ->
  if List.is_empty values
  then ()
  else (
    let len = List.length values in
    let arr = init len ~f:(fun i -> i * 2) in
    List.iter (List.range 0 len) ~f:(fun i ->
      require_equal (module Int) (get arr i) (i * 2)))
;;

let%quick_test "sexp roundtrip" =
  fun (arr : int t) ->
  let len = length arr in
  let sexp = sexp_of_t Int.sexp_of_t arr in
  let arr2 = t_of_sexp Int.t_of_sexp sexp in
  require_equal (module Int) (length arr) (length arr2);
  for i = 0 to len - 1 do
    require_equal (module Int) (get arr i) (get arr2 i)
  done
;;

let%quick_test "compare reflexive" =
  fun (arr : int t) ->
  let cmp = compare Int.compare arr arr in
  require_equal (module Int) cmp 0
;;

let%quick_test "compare antisymmetric" =
  fun (arr1 : int t) (arr2 : int t) ->
  let cmp12 = compare Int.compare arr1 arr2 in
  let cmp21 = compare Int.compare arr2 arr1 in
  require_equal (module Int) (Int.compare cmp12 0) (Int.compare 0 cmp21)
;;

let%quick_test "compare length ordering" =
  fun (arr1 : int t) (arr2 : int t) ->
  let len1 = length arr1 in
  let len2 = length arr2 in
  let cmp = compare Int.compare arr1 arr2 in
  let len_cmp = Int.compare len1 len2 in
  if len_cmp <> 0
  then require_equal (module Int) (Int.compare cmp 0) (Int.compare len_cmp 0)
;;

let%quick_test "equal reflexive" = fun (arr : int t) -> require (equal Int.equal arr arr)

let%quick_test "equal symmetric" =
  fun (arr1 : int t) (arr2 : int t) ->
  let eq12 = equal Int.equal arr1 arr2 in
  let eq21 = equal Int.equal arr2 arr1 in
  require_equal (module Bool) eq12 eq21
;;

let%quick_test "equal implies compare zero" =
  fun (arr1 : int t) (arr2 : int t) ->
  let eq = equal Int.equal arr1 arr2 in
  let cmp = compare Int.compare arr1 arr2 in
  if eq then require_equal (module Int) cmp 0
;;

let%quick_test "equal different lengths" =
  fun (arr : int t) (extra_val : int) ->
  if length arr = 0
  then ()
  else (
    let len = length arr in
    let arr2 = init (len + 1) ~f:(fun i -> if i < len then get arr i else extra_val) in
    require (not (equal Int.equal arr arr2)))
;;

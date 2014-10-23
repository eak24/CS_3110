open Assertions
open Mutability

TEST_UNIT  "count_up_from" = 
  let c = count_up_from 3 2 in
  let z = count_up_from 3 0 in
  let b = count_up_from 3 (-1) in
  assert_true(c()=3);
  assert_true(c()=5);
  assert_true(c()=7);
  assert_true(z()=3);
  assert_true(z()=3);
  assert_true(b()=3);
  assert_true(b()=2);
  ()

TEST_UNIT  "tabulate" = 
  assert_true ((tabulate (fun x-> x) 5) = [|0;1;2;3;4|]);
  assert_true ((tabulate (fun x -> x*x) 5) = [|0; 1; 4; 9; 16|]);
  ()

TEST_UNIT  "fold_left_imp" = 
  assert_true ((fold_left_imp (+) 0 [1;2;3;4]) = 10);
  assert_true ((fold_left_imp (fun a x -> x::a) [] [1;2;3;4]) = [4;3;2;1]);
  ()  

TEST_UNIT  "exercise 4" = 
  assert_false (List.map zardoz (List.rev lst) = 
    List.rev (List.map zardoz lst));
  ()

let () = Pa_ounit_lib.Runtime.summarize ()
open Assertions
open Quadtree
(*open City_search*)

(*TODO:
*EXCEPTION RAISING TESTS
*CATCH INVALID REGION EXCEPTIONS IN INSERT BY APPENDING LIST...*)

let r1 = ((0.0,0.0),(1.0,1.0))
let r2 = ((-.1.0,0.0),(0.0,1.0))
let r3 = ((-.1.0,-.1.0),(0.0,0.0))
let r4 = ((0.0,-.1.0),(1.0,0.0))
let rf = ((-.1.0,-.1.0),(1.0,1.0))

let t1 = Leaf (r1, [])
let t2 = Leaf (r2, [])
let t3 = Leaf (r3, [])
let t4 = Leaf (r4, [])
let tf = Node (rf,t1,t2,t3,t4)

TEST_UNIT "valid_region_test1" = assert_true (valid_region r1)

TEST_UNIT "valid_region_test2" = assert_false (valid_region 
  ((1.0,1.0),(0.0,0.0)))

TEST_UNIT "valid_region_test3" = assert_false (valid_region 
  ((0.0,0.0),(0.0,0.0)))

TEST_UNIT "valid_region_test4" = assert_false (valid_region 
  ((1.0,0.),(0.0,1.0)))

TEST_UNIT "new_tree_test1" = assert_true (Leaf (r1,[]) 
  = new_tree r1)

TEST_UNIT "new_tree_test2" = assert_raises (Some InvalidRegion) 
new_tree ((1.0,1.0),(0.0,0.0))

TEST_UNIT "new_tree_test2" = assert_true (Leaf (((-.1.0,-.1.0),(1.0,1.0)),[]) 
  = new_tree ((-.1.0,-.1.0),(1.0,1.0)))

(*Test empty leaf insertion*)
TEST_UNIT "insert_test1" = assert_true (Leaf (((-.1.0,-.1.0),(1.0,1.0)),
  [(0.0,0.0),5]) = insert (Leaf (((-.1.0,-.1.0),(1.0,1.0)),[])) (0.0,0.0) 5)

(*Test full leaf insertion when region is too small for node*)
TEST_UNIT "insert_test2" = assert_true 
(Leaf (((0.0,0.0),(0.0000001,0.0000001)),
  [((0.0,0.0),5);((0.0,0.0),5)]) = insert 
(Leaf (((0.0,0.0),(0.0000001,0.0000001)),
  [((0.0,0.0),5)])) (0.0,0.0) 5)

(*Test node quadrant i border insertion*)
TEST_UNIT "insert_test3" = assert_true (Node (rf,(Leaf 
  (r1, [((0.0,1.0),5)])),t2,t3,t4) = 
  insert (Node (rf,t1,t2,t3,t4)) (0.0,1.0) 5)

(*Test node quadrant i insertion*)
TEST_UNIT "insert_test4" = assert_true (Node (rf,(Leaf 
  (r1, [((0.0,0.0),5)])),t2,t3,t4) = 
  insert (Node (rf,t1,t2,t3,t4)) (0.0,0.0) 5)

(*Test node quadrant ii insertion*)
TEST_UNIT "insert_test5" = assert_true (Node (rf,t1,(Leaf 
  (r2, [((-.0.5,0.0),5)])),t3,t4) = 
  insert (Node (rf,t1,t2,t3,t4)) (-.0.5,0.0) 5)

(*Test node quadrant iii insertion*)
TEST_UNIT "insert_test6" = assert_true (Node (rf,t1,t2,(Leaf 
  (r3, [((-.0.5,-.0.5),5)])),t4) = 
  insert (Node (rf,t1,t2,t3,t4)) (-.0.5,-.0.5) 5)

(*Test node quadrant iv insertion*)
TEST_UNIT "insert_test7" = assert_true (Node (rf,t1,t2,t3,(Leaf 
  (r4, [((0.5,-.0.5),5)]))) = 
  insert (Node (rf,t1,t2,t3,t4)) (0.5,-.0.5) 5)

let tf1 = insert (Node (rf,t1,t2,t3,t4)) (0.5,0.5) 5
(*Test node quadrant i insertion when leaf full*)
TEST_UNIT "insert_test8" = assert_true (insert (tf1) (0.25,0.25) 5 = 
  Node (((-1., -1.), (1., 1.)),                                              
    Node (((0., 0.), (1., 1.)),                                              
      Leaf (((0.5, 0.5), (1., 1.)), [((0.5, 0.5), 5)]),
      Leaf (((0., 0.5), (0.5, 1.)), []),
      Leaf (((0., 0.), (0.5, 0.5)), [((0.25, 0.25), 5)]),
  Leaf (((0.5, 0.), (1., 0.5)), [])),
  Leaf (((-1., 0.), (0., 1.)), []), 
  Leaf (((-1., -1.), (0., 0.)), []),
  Leaf (((0., -1.), (1., 0.)), [])))

let tf2 = insert (Node (rf,t1,t2,t3,t4)) (-.0.5,0.5) 5
(*Test node quadrant ii insertion when leaf full*)
TEST_UNIT "insert_test9" = assert_true (insert (tf2) (-.0.25,0.25) 5 = 
  Node (((-1., -1.), (1., 1.)), 
    Leaf (((0., 0.), (1., 1.)), []),             
    Node (((-1., 0.), (0., 1.)),                                             
      Leaf (((-0.5, 0.5), (0., 1.)), [((-0.5, 0.5), 5)]),
      Leaf (((-1., 0.5), (-0.5, 1.)), []),
      Leaf (((-1., 0.), (-0.5, 0.5)), []),
      Leaf (((-0.5, 0.), (0., 0.5)), [((-0.25, 0.25), 5)])),
    Leaf (((-1., -1.), (0., 0.)), []), 
    Leaf (((0., -1.), (1., 0.)), [])))

let tf3 = insert (Node (rf,t1,t2,t3,t4)) (-.0.5,-.0.5) 5
(*Test node quadrant iii insertion when leaf full*)
TEST_UNIT "insert_test10" = assert_true (insert (tf3) (-.0.25,-.0.25) 5 = 
  Node (((-1., -1.), (1., 1.)), 
    Leaf (((0., 0.), (1., 1.)), []),             
    Leaf (((-1., 0.), (0., 1.)), []),                                        
    Node (((-1., -1.), (0., 0.)),
      Node (((-0.5, -0.5), (0., 0.)),
      Leaf (((-0.25, -0.25), (0., 0.)), [((-0.25, -0.25), 5)]),
      Leaf (((-0.5, -0.25), (-0.25, 0.)), []),
      Leaf (((-0.5, -0.5), (-0.25, -0.25)), [((-0.5, -0.5), 5)]),
      Leaf (((-0.25, -0.5), (0., -0.25)), [])),
    Leaf (((-1., -0.5), (-0.5, 0.)), []),
    Leaf (((-1., -1.), (-0.5, -0.5)), []),
  Leaf (((-0.5, -1.), (0., -0.5)), [])),
  Leaf (((0., -1.), (1., 0.)), [])))

let tf4 = insert (Node (rf,t1,t2,t3,t4)) (0.5,-.0.5) 5
(*Test node quadrant iv insertion when leaf full*)
TEST_UNIT "insert_test11" = assert_true (insert (tf4) (0.25,-.0.25) 5 = 
  Node (((-1., -1.), (1., 1.)), 
    Leaf (((0., 0.), (1., 1.)), []),  
    Leaf (((-1., 0.), (0., 1.)), []), 
    Leaf (((-1., -1.), (0., 0.)), []),      
    Node (((0., -1.), (1., 0.)),                                               
      Leaf (((0.5, -0.5), (1., 0.)), [((0.5, -0.5), 5)]),
      Leaf (((0., -0.5), (0.5, 0.)), [((0.25, -0.25), 5)]),
      Leaf (((0., -1.), (0.5, -0.5)), []),
      Leaf (((0.5, -1.), (1., -0.5)), []))))

(*Test OutOfBounds Exception for empty leaf x too low*)
TEST_UNIT "insert_test12" = assert_raises (Some OutOfBounds) 
  (insert (t1) (-.2.0,0.0)) 5

(*Test OutOfBounds Exception for empty leaf x too high*)
TEST_UNIT "insert_test13" = assert_raises (Some OutOfBounds) 
  (insert (t1) (2.0,0.0)) 5

(*Test OutOfBounds Exception for empty leaf y too low*)
TEST_UNIT "insert_test14" = assert_raises (Some OutOfBounds) 
  (insert (t1) (0.0,-.2.0)) 5

(*Test OutOfBounds Exception for empty leaf y too high*)
TEST_UNIT "insert_test15" = assert_raises (Some OutOfBounds) 
  (insert (t1) (0.0,2.0)) 5

let t1' = insert t1 (0.0,0.0) 5

(*Test OutOfBounds Exception for full leaf x too low*)
TEST_UNIT "insert_test12" = assert_raises (Some OutOfBounds) 
  (insert (t1') (-.2.0,0.0)) 5

(*Test OutOfBounds Exception for full leaf x too high*)
TEST_UNIT "insert_test13" = assert_raises (Some OutOfBounds) 
  (insert (t1') (2.0,0.0)) 5

(*Test OutOfBounds Exception for full leaf y too low*)
TEST_UNIT "insert_test14" = assert_raises (Some OutOfBounds) 
  (insert (t1') (0.0,-.2.0)) 5

(*Test OutOfBounds Exception for full leaf y too high*)
TEST_UNIT "insert_test15" = assert_raises (Some OutOfBounds) 
  (insert (t1') (0.0,2.0)) 5  

(*Test OutOfBounds Exception for Node x too low*)
TEST_UNIT "insert_test16" = assert_raises (Some OutOfBounds) 
  (insert (tf) (-.2.0,0.0)) 5

(*Test OutOfBounds Exception for Node x too high*)
TEST_UNIT "insert_test17" = assert_raises (Some OutOfBounds) 
  (insert (tf) (2.0,0.0)) 5

(*Test OutOfBounds Exception for Node y too low*)
TEST_UNIT "insert_test18" = assert_raises (Some OutOfBounds) 
  (insert (tf) (0.0,-.2.0)) 5

(*Test OutOfBounds Exception for Node y too high*)
TEST_UNIT "insert_test19" = assert_raises (Some OutOfBounds) 
  (insert (tf) (0.0,2.0)) 5

(*Test for string trees*)
TEST_UNIT "insert_test20" = assert_true (Leaf (r1,[((0.0,1.0),"hi")]) = 
  insert (t1) (0.0,1.0) "hi")

let s1 = insert (insert (insert (insert tf (1.0,0.0) "hi") (-.0.5,1.0) "howdy")
  (0.5,0.5) "wassup") (0.3,0.3) "Topothemornin"

(*test for strings list*)
TEST_UNIT "fold_quad_test1" = assert_true (fold_quad (fun acc (c,x) 
  -> x::acc) [] s1 = ["wassup"; "Topothemornin"; "hi"; "howdy"]) 

(*test for strings conc*)
TEST_UNIT "fold_quad_test2" = assert_true (fold_quad (fun acc (c,x) 
  -> x^acc) "" s1 = "wassupTopothemorninhihowdy") 
 
 let ti = insert (insert (insert (insert tf (1.0,0.0) 3) (-.0.5,1.0) 5)
  (0.5,0.5) 9) (0.3,0.3) 7

(*Test for ints list*)
TEST_UNIT "fold_quad_test3" = assert_true (fold_quad (fun acc (c,x) 
  -> x::acc) [] ti = [9; 7; 3; 5])

(*Test for ints add*)
TEST_UNIT "fold_quad_test4" = assert_true (fold_quad (fun acc (c,x) 
  -> x+acc) 0 ti = 24)

let ti2 = insert (insert (insert (insert (insert (insert (insert 
  (insert tf (0.5,0.5) 1.5) (-.0.5,0.5) 2.5) (-.0.5,-.0.5) 3.5) 
  (0.5,-.0.5) 4.5) (1.0,1.0) 1.0) (-.1.0,1.0) 2.0)
  (-.1.0,-.1.0) 3.0) (1.0,-.1.0) 4.0

(*Test for region 1*)
TEST_UNIT "fold_region_test1" = assert_true (fold_region (fun acc (c,x) 
  -> x+.acc) 0. ti2 r1 = 2.5)

(*Test for center region*)
TEST_UNIT "fold_region_test1" = assert_true (fold_region (fun acc (c,x) 
  -> x+.acc) 0. ti2 ((-.0.5,-.0.5),(0.5,0.5)) = 12.)



let () = Pa_ounit_lib.Runtime.summarize ()
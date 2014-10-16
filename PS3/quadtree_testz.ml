open Quadtree
open Assertions

let t00=new_tree ((0.,0.),(2.,2.))
let t01=(insert t00 (0.5,0.5) 1)
let t11=new_tree ((1.,1.),(2.,2.)) 
let t11'=insert t11 (1.5,1.5) 2
let t12=new_tree ((0.,1.),(1.,2.))
let t13=new_tree ((0.,0.),(1.,1.)) 
let t13'=insert t13 (0.5,0.5) 1
let t14=new_tree ((1.,0.),(2.,1.))
let t02=insert t01 (1.5,1.5) 2
let t12'=insert t12 (0.5,1.5) 3
let t03=insert t02 (0.5,1.5) 3

(*start test new*)
TEST_UNIT "new_tree_test1"= assert_true (t00= Leaf (((0.,0.),(2.,2.)),[]))
(*should raise exception when region is bad*)
TEST_UNIT "new_tree_test2"=assert_raises (Some OutOfBounds) new_tree 
((2.,2.),(0.,0.))

(*start test insert*)

TEST_UNIT "insert_test1"=assert_true (t01=Leaf (((0.,0.),(2.,2.)),[((0.5,0.5),1)]))

TEST_UNIT "insert_test2"=assert_true ((t02)=
	Node (((0.,0.),(2.,2.)),t11',t12,t13',t14))
TEST_UNIT "insert_test3"=assert_raises (Some (OutOfBounds))
    (insert (t02) (4.3,4.3)) 65536
TEST_UNIT "insert_test4"=assert_true ((t03)=
	Node (((0.,0.),(2.,2.)),t11',t12',t13',t14))
(*start test fold_quad*)
TEST_UNIT "fold_quad_test1"=assert_true (
	(fold_quad (fun x (c,s)->x+s) 0 t03)=6)
(*use different function*)
let s00=new_tree ((0.,0.),(2.,2.))
let s01=(insert s00 (0.5,0.5) "1")
let s11=new_tree ((1.,1.),(2.,2.)) 
let s11'=insert s11 (1.5,1.5) "2"
let s12=new_tree ((0.,1.),(1.,2.))
let s13=new_tree ((0.,0.),(1.,1.)) 
let s13'=insert s13 (0.5,0.5) "1"
let s14=new_tree ((1.,0.),(2.,1.))
let s02=insert s01 (1.5,1.5) "2"
let s12'=insert s12 (0.5,1.5) "3"
let s03=insert s02 (0.5,1.5) "3"
TEST_UNIT "fold_quad_test2"=assert_true (
    (fold_quad (fun x (c,s)->x^s) "" s03)="231")
TEST_UNIT "fold_quad_test3"=assert_true (
    (fold_quad (fun x (c,s)->x^s) "" s00)="")
(*test fold_region*)
TEST_UNIT "fold_region_test1"=assert_true(
    (fold_region (fun x (c,s)->x^s) "" s03 ((0.,0.),(2.,2.)))="231")
TEST_UNIT "fold_region_test2"=assert_true(
	(fold_region (fun x (c,s)->x+s) 0 t03 ((0.,0.),(1.,1.)))=1)
TEST_UNIT "fold_region_test3"=assert_true((
     fold_region (fun x (c,s)->x+s) 0 t11' ((0.,0.),(2.,1.)) )=0)
TEST_UNIT "fold_region_test4"=assert_raises (Some OutOfBounds)
    (fold_region (fun x (c,s)->x+s) 0 t03) ((1.,1.),(0.,0.))
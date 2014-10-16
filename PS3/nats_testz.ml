open Nats
open Assertions

(*IntNat*)
(*basic conversion*)
TEST_UNIT "IntNat_test0"=assert_raises (Some IntNat.Unrepresentable)
IntNat.nat_of_int (-1)
TEST_UNIT "IntNat_test1"=assert_true 
((IntNat.int_of_nat IntNat.zero)=0 )
TEST_UNIT "IntNat_test2"=assert_true 
((IntNat.int_of_nat IntNat.one)=1 )
TEST_UNIT "IntNat_test3"=assert_true 
((IntNat.nat_of_int 0)=IntNat.zero )
TEST_UNIT "IntNat_test4"=assert_true 
((IntNat.nat_of_int 1)=IntNat.one )
(*basic conversion other than units*)
TEST_UNIT "IntNat_test5"=assert_true 
((IntNat.int_of_nat (IntNat.nat_of_int 2))=2)
(*plus with/without overflow*)
TEST_UNIT "IntNat_test6"=assert_true 
((IntNat.int_of_nat 
(IntNat.(+) (IntNat.nat_of_int 2) (IntNat.nat_of_int 3) ))=5)
(*in the case of 32 bit*)
TEST_UNIT "IntNat_test7"=assert_raises (Some IntNat.Unrepresentable)
(IntNat.(+) (IntNat.nat_of_int 1000000000)) 
(IntNat.nat_of_int 1000000000)
(*multiplication with/without overflow*)
TEST_UNIT "IntNat_test8"=assert_true 
((IntNat.int_of_nat
(IntNat.( * ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 2) )=6))
TEST_UNIT "IntNat_test9"=assert_raises (Some IntNat.Unrepresentable)
(IntNat.( * ) (IntNat.nat_of_int 10000000))
(IntNat.nat_of_int 1000000)
(*<*)
TEST_UNIT "IntNat_test10"=assert_true
((IntNat.( < ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 2))
=false)
TEST_UNIT "IntNat_test11"=assert_true
(IntNat.( < ) (IntNat.nat_of_int 2) (IntNat.nat_of_int 3))
TEST_UNIT "IntNat_test12"=assert_false
((IntNat.( < ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 3)))
(*===*)
TEST_UNIT "IntNat_test13"=assert_true
((IntNat.( === ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 2))
=false)
TEST_UNIT "IntNat_test14"=assert_true
((IntNat.( === ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 4))
=false)
TEST_UNIT "IntNat_test15"=assert_true
(IntNat.( === ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 3))
(*commutative plus/ multiplication*)
TEST_UNIT "IntNat_test16"=assert_true
(IntNat.(===) (IntNat.(+) (IntNat.nat_of_int 3) (IntNat.nat_of_int 5)) 
(IntNat.( + ) (IntNat.nat_of_int 5) (IntNat.nat_of_int 3)))
TEST_UNIT "IntNat_test17"=assert_true
(IntNat.(===) (IntNat.( * ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 5)) 
(IntNat.( * ) (IntNat.nat_of_int 5) (IntNat.nat_of_int 3)))
(*associative plus/multiplication*)
TEST_UNIT "IntNat_test18"=assert_true
(IntNat.(===) 
(IntNat.(+) (IntNat.(+) (IntNat.nat_of_int 2) 
	(IntNat.nat_of_int 3)) (IntNat.nat_of_int 4))
(IntNat.(+) (IntNat.nat_of_int 2) 
	(IntNat.(+) (IntNat.nat_of_int 3) (IntNat.nat_of_int 4))))
TEST_UNIT "IntNat_test19"=assert_true
(IntNat.(===) 
(IntNat.( * ) (IntNat.( * ) (IntNat.nat_of_int 2) 
	(IntNat.nat_of_int 3)) (IntNat.nat_of_int 4))
(IntNat.( * ) (IntNat.nat_of_int 2) 
	(IntNat.( * ) (IntNat.nat_of_int 3) (IntNat.nat_of_int 4))))

(*ListNat*)
(*basic conversion*)
TEST_UNIT "ListNat_test0"=assert_raises (Some ListNat.Unrepresentable)
ListNat.nat_of_int (-1)
TEST_UNIT "ListNat_test1"=assert_true 
((ListNat.int_of_nat ListNat.zero)=0 )
TEST_UNIT "ListNat_test2"=assert_true 
((ListNat.int_of_nat ListNat.one)=1 )
TEST_UNIT "ListNat_test3"=assert_true 
((ListNat.nat_of_int 0)=ListNat.zero )
TEST_UNIT "ListNat_test4"=assert_true 
((ListNat.nat_of_int 1)=ListNat.one )
(*basic conversion other than units*)
TEST_UNIT "ListNat_test5"=assert_true 
((ListNat.int_of_nat (ListNat.nat_of_int 2))=2)
(*plus without overflow*)
TEST_UNIT "ListNat_test6"=assert_true 
((ListNat.int_of_nat 
(ListNat.(+) (ListNat.nat_of_int 2) (ListNat.nat_of_int 3) ))=5)


(*multiplication without overflow*)
TEST_UNIT "ListNat_test8"=assert_true 
((ListNat.int_of_nat
(ListNat.( * ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 2) )=6))

(*<*)
TEST_UNIT "ListNat_test10"=assert_true
((ListNat.( < ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 2))
=false)
TEST_UNIT "ListNat_test11"=assert_true
(ListNat.( < ) (ListNat.nat_of_int 2) (ListNat.nat_of_int 3))
TEST_UNIT "ListNat_test12"=assert_false
((ListNat.( < ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 3)))
(*===*)
TEST_UNIT "ListNat_test13"=assert_true
((ListNat.( === ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 2))
=false)
TEST_UNIT "ListNat_test14"=assert_true
((ListNat.( === ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 4))
=false)
TEST_UNIT "ListNat_test15"=assert_true
(ListNat.( === ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 3))
(*commutative plus/ multiplication*)
TEST_UNIT "ListNat_test16"=assert_true
(ListNat.(===) (ListNat.(+) (ListNat.nat_of_int 3) (ListNat.nat_of_int 5)) 
(ListNat.( + ) (ListNat.nat_of_int 5) (ListNat.nat_of_int 3)))
TEST_UNIT "ListNat_test17"=assert_true
(ListNat.(===) (ListNat.( * ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 5)) 
(ListNat.( * ) (ListNat.nat_of_int 5) (ListNat.nat_of_int 3)))
(*associative plus/multiplication*)
TEST_UNIT "ListNat_test18"=assert_true
(ListNat.(===) 
(ListNat.(+) (ListNat.(+) (ListNat.nat_of_int 2) 
	(ListNat.nat_of_int 3)) (ListNat.nat_of_int 4))
(ListNat.(+) (ListNat.nat_of_int 2) 
	(ListNat.(+) (ListNat.nat_of_int 3) (ListNat.nat_of_int 4))))
TEST_UNIT "ListNat_test19"=assert_true
(ListNat.(===) 
(ListNat.( * ) (ListNat.( * ) (ListNat.nat_of_int 2) 
	(ListNat.nat_of_int 3)) (ListNat.nat_of_int 4))
(ListNat.( * ) (ListNat.nat_of_int 2) 
	(ListNat.( * ) (ListNat.nat_of_int 3) (ListNat.nat_of_int 4))))

(*NatConvertFn*)
module U=NatConvertFn (IntNat)
module V=NatConvertFn (ListNat)
TEST_UNIT "NatConvertFn_test1"=assert_true
((U.int_of_nat (U.nat_of_int 3))=3)
TEST_UNIT "NatConvertFn_test2"=assert_true
((V.int_of_nat (V.nat_of_int 3))=3)
(*they will throw Unrepresentable if input<-1*)
TEST_UNIT "NatConvertFn_test3"=assert_raises
(Some IntNat.Unrepresentable) U.nat_of_int (-1)
TEST_UNIT "NatConvertFn_test4"=assert_raises
(Some ListNat.Unrepresentable) V.nat_of_int (-1)

type alie = N|Ich

module A: (AlienMapping with type aliensym=alie)= struct
  type aliensym = alie
  let int_of_aliensym x= if x=N then 0 else 1
  let one=Ich
  let zero=N
end
(*AlienNatFn*)
module Omega=  AlienNatFn (A) 

(*basic conversion*)
TEST_UNIT "AlienNatFn_test0"=assert_raises (Some Omega.Unrepresentable)
Omega.nat_of_int (-1)
TEST_UNIT "AlienNatFn_test1"=assert_true 
((Omega.int_of_nat Omega.zero)=0 )
TEST_UNIT "AlienNatFn_test2"=assert_true 
((Omega.int_of_nat Omega.one)=1 )
TEST_UNIT "AlienNatFn_test3"=assert_true 
((Omega.nat_of_int 0)=Omega.zero )
TEST_UNIT "AlienNatFn_test4"=assert_true 
((Omega.nat_of_int 1)=Omega.one )
(*basic conversion other than units*)
TEST_UNIT "AlienNatFn_test5"=assert_true 
((Omega.int_of_nat (Omega.nat_of_int 2))=2)
(*plus without overflow*)
TEST_UNIT "AlienNatFn_test6"=assert_true 
((Omega.int_of_nat 
(Omega.(+) (Omega.nat_of_int 2) (Omega.nat_of_int 3) ))=5)
(*multiplication without overflow*)
TEST_UNIT "AlienNatFn_test8"=assert_true 
((Omega.int_of_nat
(Omega.( * ) (Omega.nat_of_int 3) (Omega.nat_of_int 2) )=6))

(*<*)
TEST_UNIT "AlienNatFn_test10"=assert_true
((Omega.( < ) (Omega.nat_of_int 3) (Omega.nat_of_int 2))
=false)
TEST_UNIT "AlienNatFn_test11"=assert_true
(Omega.( < ) (Omega.nat_of_int 2) (Omega.nat_of_int 3))
TEST_UNIT "AlienNatFn_test12"=assert_false
((Omega.( < ) (Omega.nat_of_int 3) (Omega.nat_of_int 3)))
(*===*)
TEST_UNIT "AlienNatFn_test13"=assert_true
((Omega.( === ) (Omega.nat_of_int 3) (Omega.nat_of_int 2))
=false)
TEST_UNIT "AlienNatFn_test14"=assert_true
((Omega.( === ) (Omega.nat_of_int 3) (Omega.nat_of_int 4))
=false)
TEST_UNIT "AlienNatFn_test15"=assert_true
(Omega.( === ) (Omega.nat_of_int 3) (Omega.nat_of_int 3))
(*commutative plus/ multiplication*)
TEST_UNIT "AlienNatFn_test16"=assert_true
(Omega.(===) (Omega.(+) (Omega.nat_of_int 3) (Omega.nat_of_int 5)) 
(Omega.( + ) (Omega.nat_of_int 5) (Omega.nat_of_int 3)))
TEST_UNIT "AlienNatFn_test17"=assert_true
(Omega.(===) (Omega.( * ) (Omega.nat_of_int 3) (Omega.nat_of_int 5)) 
(Omega.( * ) (Omega.nat_of_int 5) (Omega.nat_of_int 3)))
(*associative plus/multiplication*)
TEST_UNIT "AlienNatFn_test18"=assert_true
(Omega.(===) 
(Omega.(+) (Omega.(+) (Omega.nat_of_int 2) 
	(Omega.nat_of_int 3)) (Omega.nat_of_int 4))
(Omega.(+) (Omega.nat_of_int 2) 
	(Omega.(+) (Omega.nat_of_int 3) (Omega.nat_of_int 4))))
TEST_UNIT "AlienNatFn_test19"=assert_true
(Omega.(===) 
(Omega.( * ) (Omega.( * ) (Omega.nat_of_int 2) 
	(Omega.nat_of_int 3)) (Omega.nat_of_int 4))
(Omega.( * ) (Omega.nat_of_int 2) 
	(Omega.( * ) (Omega.nat_of_int 3) (Omega.nat_of_int 4))))



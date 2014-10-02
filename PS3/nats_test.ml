open Assertions
open Nats.IntNat

TEST_UNIT "IntNat.zero" =
	assert_true(nat_of_int(0)=zero);
	assert_true(int_of_nat(zero)=0);
	assert_true(zero*nat_of_int(1)=zero);
	assert_true(zero+nat_of_int(1)=nat_of_int(1));
	()

TEST_UNIT "IntNat.one" = 
	assert_true(nat_of_int(1)=one);
	assert_true(int_of_nat(one)=1);
	assert_true(one*nat_of_int(3)=nat_of_int(3));
	assert_true(one+nat_of_int(1)=nat_of_int(2));
	()

let x2 = nat_of_int(2)
let x3 = nat_of_int(3)
let x5 = nat_of_int(5)
let m2x = nat_of_int(max_int/2)
let mx = nat_of_int(max_int)

TEST_UNIT " ( + ) " = 
	assert_true(x2+x3=x5); (*Commutativity*)
	assert_true(x3+x2=x5); (*Commutativity*)
	assert_true(one+x2=x3); (*Adding one*)
	assert_true(zero+x2=x2); (*Identity*)
	assert_true(one+zero=one); (*With the special values*)
	assert_true(one+one+one=x3); (*Multiple in a row*)
	assert_true((x2+x3)+x3=(x2+(x3+x3))); (*Associativity*)
	assert_raises (Some Unrepresentable) (( + ) (nat_of_int(max_int))) one;
	assert_true (mx+zero=mx); (*Sum equallying max_int*)
	assert_true (m2x+m2x+one=mx); 
	assert_true (m2x+one+m2x=mx);
	assert_true (zero+zero=zero);
	()

TEST_UNIT " ( * ) " = ()

TEST_UNIT " ( < ) " = ()

TEST_UNIT " ( === ) " = ()

TEST_UNIT " nat_of_int " = ()

TEST_UNIT " int_of_nat " = ()

TEST_UNIT " mult_overflows " = 
	assert_false(false);
	()


let () = Pa_ounit_lib.Runtime.summarize () 
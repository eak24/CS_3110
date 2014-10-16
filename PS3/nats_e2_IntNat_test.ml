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

(*The values are used throughout the rest of the test cases.*)
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

TEST_UNIT " ( * ) " = 
	(*v Below v Raise Errors for overflow.*)
	assert_raises (Some Unrepresentable) (( * ) mx) mx;
	assert_raises (Some Unrepresentable) (( * ) m2x) m2x;
	assert_raises (Some Unrepresentable) (( * ) mx) x2;
	assert_raises (Some Unrepresentable) (( * ) (m2x+one)) x2;
	assert_true ((m2x*x2)+one=mx); (*Get close to product being max_int*)
	assert_true (zero*zero=zero); (*Mult by zero*)
	assert_true (zero*one=zero); 
	(*^above^ Identity of one, Special Values, Commutativity, Multiply by zero*)
	assert_true (one*zero=zero); 
	(*^above^ Identity of one, Special Values, Commutativity, Multiply by zero*)
	assert_true (one*x3=x3); (*Identity of one, Commutativity*)
	assert_true (x3*one=x3); (*Identity of one, Commutativity*)
	assert_true (x2*(x2+x3)=(x2*x2)+(x2*x3)); (* Distributivity *)
	assert_true (x5*x2*(x2+x3)=(x2*x2*x5)+(x2*x5*x3)); 
	(*^above^ Distributitivy, Commutativity, Multiple ( * ) in a row *)
	()

TEST_UNIT " ( < ) " = 
	assert_true(zero<one);
	assert_true(one<x2);
	assert_true(x2<x3);
	assert_true(zero<x3);
	assert_false(zero<zero);
	assert_false(one<zero);
	assert_false(x3<x2);
	assert_true(m2x<mx);
	()

TEST_UNIT " ( === ) " =
	assert_true(x2===(one+one));
	assert_true((one+one)===x2);
	assert_false(zero===one);
	assert_false(m2x===mx);
	assert_false(mx===m2x);
	()

TEST_UNIT " nat_of_int " =
	(*Raises Unrepresentable  *)
	assert_raises (Some Unrepresentable) nat_of_int (~-1); 
	assert_true(nat_of_int(0)=zero);
	assert_true(nat_of_int(1)=one);
	assert_true(one+one=nat_of_int(2));
	assert_true((one+one)*(one+one)=nat_of_int(4));
	assert_true(int_of_nat(nat_of_int(max_int))=max_int);
	assert_true(nat_of_int(8)+nat_of_int(12)=nat_of_int(20));
	()

TEST_UNIT " int_of_nat " = 
	assert_true(int_of_nat(zero)=0);
	assert_true(int_of_nat(one)=1);
	assert_true(int_of_nat(mx)=max_int);
	assert_true(int_of_nat(m2x)=(max_int/2));
	assert_true(int_of_nat(x2)=2);
	assert_true(int_of_nat(x3)=3);
	assert_true(int_of_nat(x5)=5);
	()

TEST_UNIT " mult_overflows " =
(*Ran these in utop but can't include here because mult_overflows not 
 * included in sig*)
	(*assert_true(IntNat.mult_overflows max_int max_int);
	assert_true(mult_overflows (max_int/2) 3);
	assert_true(mult_overflows (max_int/3) 5);
	assert_false(mult_overflows (max_int/2) 2);
	assert_false(mult_overflows  2 (max_int/2));
	assert_false(mult_overflows 0 1);
	assert_false(mult_overflows 1 0);
	assert_false(mult_overflows 0 0);
	assert_false(mult_overflows 10 53); *)
	()

let () = Pa_ounit_lib.Runtime.summarize () 
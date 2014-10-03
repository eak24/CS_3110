open Assertions 
open Nats

type alie =  Ethan | Andrea | Zichuan
type mmm = alie list



module M: (AlienMapping with type aliensym=mmm)= struct
  type aliensym = mmm

  let rec int_of_aliensym (x:aliensym):int= 
  	match x with 
  	| [] -> 0
  	|[Ethan::t] -> 0 + int_of_aliensym(t)
  	|[Andrea::t] -> 1 + int_of_aliensym(t)
  	|[Zichuan::t] -> 2 + int_of_aliensym(t)
  let zero=[Ethan]
  let one=[Andrea]
  
end

module MIRP = AlienNatFn (M) 

open MIRP

TEST_UNIT "MIRP.zero" =
	assert_true(MIRP.nat_of_int(0)=MIRP.zero);
	assert_true(MIRP.int_of_nat(zero)=0);
	assert_true(MIRP.zero*nat_of_int(1)=MIRP.zero);
	assert_true(MIRP.zero+nat_of_int(1)=MIRP.nat_of_int(1));
	()

TEST_UNIT "MIRP.one" = 
	assert_true(nat_of_int(1)=one);
	assert_true(int_of_nat(one)=1);
	assert_true(one*nat_of_int(3)=nat_of_int(3));
	assert_true(one+nat_of_int(1)=nat_of_int(2));
	()

(*The values are used throughout the rest of the test cases.*)
let x2 = nat_of_int(2)
let x3 = nat_of_int(3)
let x5 = nat_of_int(5)
(* Originally, mx represented nat_of_int(max_int), 
 * but the process got killed, 
 * and this is the largest we can get without it getting killed *)
let m2x = nat_of_int(100)
let mx =  nat_of_int(1000)
let mi = 1000

TEST_UNIT " ( + ) " = 
	assert_true(x2+x3=x5); (*Commutativity*)
	assert_true(x3+x2=x5); (*Commutativity*)
	assert_true(one+x2=x3); (*Adding one*)
	assert_true(zero+x2=x2); (*Identity*)
	assert_true(one+zero=one); (*With the special values*)
	assert_true(one+one+one=x3); (*Multiple in a row*)
	assert_true((x2+x3)+x3=(x2+(x3+x3))); (*Associativity*)
	(* Can't test.
		assert_raises (Some Unrepresentable) (( + ) (nat_of_int(mi))) one; *)
	assert_true (mx+zero=mx); (*Sum equallying max_int*)
	assert_true (x2+x2+one=x5); 
	assert_true (x2+one+x2=x5);
	assert_true (zero+zero=zero);
	()

TEST_UNIT " ( * ) " = 
	(*v Below v Raise Errors for overflow.*)
	(* Can't be tested because process is killed long before reaching numbers
		that big.
	assert_raises (Some Unrepresentable) (( * ) mx mx;
	assert_raises (Some Unrepresentable) (( * ) m2x) m2x;
	assert_raises (Some Unrepresentable) (( * ) mx) x2;
	assert_raises (Some Unrepresentable) (( * ) (m2x+one)) x2; 
	assert_true ((m2x*x2)+one=mx); (*Get close to product being max_int*) *)
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
	assert_true(int_of_nat(nat_of_int(mi))=mi);
	assert_true(nat_of_int(8)+nat_of_int(12)=nat_of_int(20));
	()

TEST_UNIT " int_of_nat " = 
	assert_true(int_of_nat(zero)=0);
	assert_true(int_of_nat(one)=1);
	assert_true(int_of_nat(mx)=mi);
	assert_true(int_of_nat(m2x)=(mi/10));
	assert_true(int_of_nat(x2)=2);
	assert_true(int_of_nat(x3)=3);
	assert_true(int_of_nat(x5)=5);
	()


let () = Pa_ounit_lib.Runtime.summarize () 
open Assertions 
open Nats
module It = NatConvertFn(Nats.IntNat)
module Lt = NatConvertFn(Nats.ListNat)

TEST_UNIT "int_of_nat and nat_of_int" =
	assert_true(0=It.int_of_nat(It.nat_of_int(0)));
	assert_true(1=Lt.int_of_nat(Lt.nat_of_int(1)));
	assert_true(0=IntNat.int_of_nat(It.nat_of_int(0)));
	assert_true(1=Lt.int_of_nat(ListNat.nat_of_int(1)));
	assert_true(0=It.int_of_nat(IntNat.nat_of_int(0)));
	assert_true(1=ListNat.int_of_nat(Lt.nat_of_int(1)));
	assert_raises (Some ListNat.Unrepresentable) Lt.nat_of_int (-1);
	assert_raises (Some IntNat.Unrepresentable) It.nat_of_int (-1);
	()


let () = Pa_ounit_lib.Runtime.summarize () 
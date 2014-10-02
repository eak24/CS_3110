open Assertions
open Nats

TEST_UNIT "IntNat.zero" =
	assert_true(nat_of_int(0)=0);
	assert_true(int_of_nat(IntNat.zero)=0);
	assert_true()

let () = Pa_ounit_lib.Runtime.summarize ()
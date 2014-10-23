open Assertions
open Iterator

TEST_UNIT "ListIterator tests" = 
  let x = ListIterator.create [] in
  let y = ListIterator.create [1] in
  let z = ListIterator.create [1;2] in
  assert_false(ListIterator.has_next x);
  assert_true(ListIterator.has_next y);
  assert_true(ListIterator.has_next z);
  assert_true((ListIterator.next y)=1);
  assert_false(ListIterator.has_next y);
  assert_true((ListIterator.next z)=1);
  assert_true(ListIterator.has_next z);
  assert_true((ListIterator.next z)=2);
  assert_false(ListIterator.has_next z);
  assert_raises (Some ListIterator.NoResult) ListIterator.next x;
  assert_raises (Some ListIterator.NoResult) ListIterator.next y;
  assert_raises (Some ListIterator.NoResult) ListIterator.next z;
  ()

TEST_UNIT "InorderTreeIterator tests" = 
  let x = InorderTreeIterator.create Leaf in
  let y = InorderTreeIterator.create (Node (1, Leaf, Leaf)) in
  let z = InorderTreeIterator.create (Node (2, Node (1, Leaf, Leaf), Leaf)) in
  let a = InorderTreeIterator.create (Node (2, (Node (1, Leaf, Leaf)),(Node (3, Leaf, Leaf)))) in
  assert_false(InorderTreeIterator.has_next x);
  assert_true(InorderTreeIterator.has_next y);
  assert_true(InorderTreeIterator.has_next z);
  assert_true((InorderTreeIterator.next y)=1);
  assert_false(InorderTreeIterator.has_next y);
  assert_true((InorderTreeIterator.next z)=1);
  assert_true(InorderTreeIterator.has_next z);
  assert_true((InorderTreeIterator.next z)=2);
  assert_false(InorderTreeIterator.has_next z);
  assert_true((InorderTreeIterator.next a)=1);
  assert_true(InorderTreeIterator.has_next a);
  assert_true((InorderTreeIterator.next a)=2);
  assert_true(InorderTreeIterator.has_next a);
  assert_true((InorderTreeIterator.next a)=3);
  assert_false(InorderTreeIterator.has_next a);
  assert_raises (Some InorderTreeIterator.NoResult) InorderTreeIterator.next x;
  assert_raises (Some InorderTreeIterator.NoResult) InorderTreeIterator.next y;
  assert_raises (Some InorderTreeIterator.NoResult) InorderTreeIterator.next z;
  assert_raises (Some InorderTreeIterator.NoResult) InorderTreeIterator.next a;
  ()

TEST_UNIT "TakeIterator tests" = 
  let lit = ListIterator.create [1;2] in
  module Tlit = TakeIterator(lit)
  let tlit = Tlit.create 1 lit in
  assert_true(Tlit.has_next tlit);
  assert_true(Tlit.next tlit = 1);
  assert_false(Tlit.has_next tlit);
  assert_raises (Some Tlit.NoResult) Tlit.next tlit;

  let tit = InorderTreeIterator.create (Node (2, Node (1, Leaf, Leaf), Leaf)) in
  module Ttit = TakeIterator(tit)
  let ttit = Ttit.create 1 tit in
  assert_true(Ttit.has_next ttit);
  assert_true(Ttit.next Ttit = 1);
  assert_false(Ttit.has_next ttit);
  assert_raises (Some Ttit.NoResult) Ttit.next ttit;

  let ttit2 = Ttit.create 0 tit in
  assert_false(Ttit.has_next ttit2);
  assert_raises (Some Ttit.NoResult) Ttit.next ttit2;
  ()

let () = Pa_ounit_lib.Runtime.summarize ()
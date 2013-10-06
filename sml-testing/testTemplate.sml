(*Loading the modules of the simple testing framework*)
use "testing.sml";
open SmlTests; (*Opening the module for testing*)

(* load your dependencies. The "use" directory is your working *)
(* directory in the command session *)
use "../hw1.sml";

(*Assert your stuff.*)
(*You also have assert_true, assert_false, assert_raises and *)
(*assert_equals (which depend on the implementation of a *)
(*formatter for your datatype)*)
test("(1,2,3 is older than (2,3,4))", 
    assert_true(
        is_older((1,2,3),(2,3,4))
    )); (*DO NOT FORGET THE SEMICOLONS!!*)

test("test14",
     assert_true(
         reasonable_date((0,0,1)) = false andalso
         reasonable_date((4,2,29)) = true andalso
         reasonable_date((2013,2,28)) = true andalso
         reasonable_date((2012,2,29)) = true andalso
         reasonable_date((2013, 22, 3)) = false andalso
         reasonable_date((2012,11,33)) = false andalso
         reasonable_date((1990, 11, 23)) = true
    ));

(*Now, I will need to run this test suite*)
run();

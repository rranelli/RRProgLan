(*Loading the modules of the simple testing framework*)
use "testing.sml";

open SmlTests;

(* Homework1 Simple Test *)
use "../hw1.sml";

test("(1,2,3 is older than (2,3,4))", 
    assert_false(
        is_older((1,2,3),(2,3,4))
    ));

test("test2",
     assert_true(
         number_in_month([(2012,2,28),(2013,12,1)],2) = 1
    ));

test("test3",
     assert_true(
         number_in_months([(2012,2,28),(2013,12,1),(2011,3,31)
                              ,(2011,4,28)],[2,3,4]) = 3
    ));

test("test4",
     assert_true(
         dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
    ));

test("test5",
     assert_false(
         dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
         = [(2012,2,28),(2011,3,31),(2011,4,28)]
));

test("test6",
     assert_true(
         get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
    ));

test("test7",
     assert_true(
         date_to_string((2013, 6, 1)) = "June 1, 2013"
    ));

test("test8",
     assert_true(
         number_before_reaching_sum(10, [1,2,3,4,5]) = 3
    ));

test("test9",
     assert_true(
         what_month(70) = 3
    ));

test("test10",
     assert_false(
         month_range(31, 34) = [1,2,2,2]
    ));

test("test11",
     assert_true(
         oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) =
         SOME (2011,3,31)
));

test("testContains",
     assert_true(
         contains(1, [3,4,5,6,7,1])
    ));

test("testUnique",
     assert_true(
         unique([1,2,3,4,5,1,1,1,2,2,3]) = [4,5,1,2,3]
    ));

test("test12",
     assert_true(
         number_in_months_challenge([(2012,2,28),(2013,12,1)
                                     ,(2011,3,31),(2011,4,28)]
                                   ,[2,3,4,4,3,2]) = 3
));

test("test13",
     assert_true(
         dates_in_months_challenge([(2012,2,28),(2013,12,1)
                                    ,(2011,3,31),(2011,4,28)]
                                  ,[2,3,4,2,3,4])
         = [(2012,2,28),(2011,3,31),(2011,4,28)]
    ));

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

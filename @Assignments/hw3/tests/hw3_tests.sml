(*Homework 3 tests*)
(*setting up references to the framework*)
use "testing.sml";
open SmlTests;
use "../hw3.sml";

test("A list with only capitals should not be modified",
     assert_equals_string_list (
         only_capitals ["A","B","C"] , ["A","B","C"]
     )
    );

test("longst 1should select the longest string",
     assert_equals_string (
         longest_string1 ["A","bc","C"] , "bc"
     )
    );

test("longst 1 should select the first string in case of tie",
     assert_equals_string (
         longest_string1 ["A","bc","xx"] , "bc"
     )
    );

test("longs2 should select the longest string",
     assert_equals_string (
         longest_string2 ["A","bc","C"] , "bc"
     )
    );

test("longst 2 should select the second string in case of tie",
     assert_equals_string (
         longest_string2 ["A","bc","xx"] , "xx"
     )
    );

test("longs3 should work just like longs1",
     assert_equals_string (
         longest_string3 ["A","bc","C"] , "bc"
     )
);

test("longs4 should work just like longs4",
     assert_equals_string (
         longest_string4 ["A","B","C"] , "C"
     )
);

test("longest capitalized should get the longest capitalized",
     assert_equals_string (
         longest_capitalized ["Abcx","bc","Cdfd_iamlongest"] , "Cdfd_iamlongest"
     )
    );

test("longest capitalized should resolve ties with the first occurence",
     assert_equals_string (
         longest_capitalized ["Abcx","bc","Cdfd"] , "Abcx"
     )
    );

test("should reverse the string correctly",
     assert_equals_string (
          rev_string ("abc"), "cba"
      )
    );

test("rev string should work for empty strings", 
     assert_equals_string (
         rev_string (""), ""
     )
);

test("first answer should get first answer", 
     assert_equals_int (
         first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] , 4
     )
);

test("all_answers shoud be NONE",
     assert_true (
         all_answers (fn x => if x = 1 then SOME [x] else NONE
                     ) [2,3,4,5,6,7] = NONE
     )
);

test("all_answers should return answers when there are",
     assert_true (
         all_answers (fn x => if x > 2 then SOME [x, x+1] else NONE
                     ) [0, 33, 2, 1, 77] = SOME [33, 34, 77, 78]
     )
    );

test ("should count a single wildcard right", 
     assert_equals_int (
         count_wildcards Wildcard, 1
     )
    );

test ("should count wildcards if they are in a list",
      assert_equals_int (
          count_wildcards (TupleP [Wildcard, Wildcard]), 2
      )
     );

test ("should count wildcards if there are other things in the pat", 
      assert_equals_int(
          count_wildcards (TupleP [Wildcard, Variable("x"), Wildcard]), 2
      )
     );

test ("should get var length",
      assert_equals_int(
          count_wild_and_variable_lengths (Variable("a")) , 1
      )
);

test ("should count wildcards and sum with var lengths",
      assert_equals_int (
          count_wild_and_variable_lengths (
              TupleP [Wildcard, Wildcard,
                      Variable("asdf"), Variable("1123"),
                      Wildcard]), 3+4+4
      )
);

test ("countsomevar should count a single var",
      assert_equals_int(
          count_some_var ("x", Variable("x")) , 1
      )
);

test ("countsomevar should count when there are different vars",
      assert_equals_int (
          count_some_var ("x", 
                         TupleP ([Variable("x"), Variable("y"),
                         Wildcard, Variable("x")])), 2
      )
);

test ("checkpat should return true if there is a single variable",
      assert_true (
          check_pat (Variable("x")) = true
      )
);

test ("checkpat should return false if there are repeated var names",
      assert_true (
          check_pat (
              TupleP ([Variable("x"), Wildcard, Variable("x")])
          ) = false
      )
     );

test ("match should match a variable binding",
      assert_true (
          match (Const(1), Variable("x")) = SOME [("x", Const(1))]
      )
     );

test ("match should match a constructor binding when they are same",
      assert_true (
          match (Constructor("myCons", Const(1)),
                 ConstructorP("myCons", Variable("x")))
          = SOME [("x", Const(1))]
      )
     );

test ("match should not match a constructor binding when they differ",
      assert_true (
          match (Constructor("myCons", Const(1)),
                 ConstructorP("myCons", Variable("x")))
          = SOME [("x", Const(1))]
      )
     );

test ("match should only match unit pattern with unit value", 
      assert_true (
          match (Unit, UnitP) = SOME [] andalso
          match (Unit, ConstP(1)) = NONE andalso
          match (Const(1), UnitP) = NONE
      )
     );

test ("match should give an empty list of bindings if" ^
      "there is no var in pattern", 
      assert_true (
          match (Constructor("myCons", Const(1)),
                 ConstructorP("myCons", Wildcard)) = SOME []
      )
     );

test ("match should match a complex and nested tuple value",
      assert_true (
          match (
              Tuple ([Const(1), Const(2), 
                      Tuple (
                          [Const(12), Constructor("k", Const(14))]
                     )]
                    ),
              TupleP ([Variable("x"), Variable("y"), 
                       TupleP (
                           [Variable("z"), ConstructorP("k", Variable("w"))]
                      )]
                     )
          )
          = SOME [("x", Const(1)),("y", Const(2)),
                  ("z", Const(12)),("w", Const(14))]
      )
     );

test ("unit should be first matched into some empty binding list", 
      assert_true (
          (first_match Unit [UnitP]) = SOME []
      )
     );

run ();

(* Homework2 Simple Test *)
use "testing.sml";
open SmlTests;

use "../hw2.sml";

test("works when the list is nil", 
     assert_true(
         all_except_option("josafh", []) = NONE
     )
    );

test("give some of nil if there " ,
     assert_true(
         all_except_option("string", ["string"]) = SOME []
     )
    );

test("give some of the list with the element removed",
     assert_true(
         all_except_option("val", ["val", "otherval"]) = SOME ["otherval"]
     )
    );

test("give some of the list with the element removed 2",
     assert_true(
         all_except_option("val", ["fucked val", "val", "otherval"]) = SOME ["fucked val", "otherval"]
     )
    );

test("test2  is right",
     assert_true(
         get_substitutions1([["foo"],["there"]], "foo") = []
     )
    );

test("Example given in the exercise",
     assert_true(get_substitutions1(
                [["Freddie", "Frederick"], ["Fucker"],["Joshua", "Freddie"]], "Freddie") = ["Frederick" , "Joshua"])
    );

test("test2  is right for tail recursive",
     assert_true(get_substitutions2([["foo"],["there"]], "foo") = [])
    );

test("Example given in the exercise for tail recursive",
     assert_true(get_substitutions2(
                [["Freddie", "Frederick"], ["Fucker"],["Joshua", "Freddie"]], "Freddie") = ["Frederick" , "Joshua"])
    );

test("test4  is right",
     assert_true(
         similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
         [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
          {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
     )
    );

test("test5  is right",
     assert_true(card_color((Clubs, (Num 2))) = Black)
    );

test("test6  is right",
     assert_true(card_value((Clubs, (Num 2))) = 2)
    );

test("test7  is right",
     assert_true(
         remove_card([(Hearts, Ace)], (Hearts, Ace),
                             IllegalMove) = []
     )
    );

test("remove the given card only ONCE", 
     assert_true(
         remove_card([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace),
                     IllegalMove) = [(Hearts, Ace)]
     )
    );

test("test8  is right",
     assert_true(
         all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
     )
    );


test("test9  is right",
     assert_true(
         sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
     )
);


test("test10 is right", 
     assert_true(
         score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
     )
    );


test("test11 is right",
     assert_true(
         officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
     )
    );

test("test12 is right",
     assert_true(
         officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                   [Draw,Draw,Draw,Draw,Draw], 42)= 3
     )
    );

test("test13 is right",
     assert_true(
         (
           (officiate([(Clubs,Jack),(Spades,Num(8))],
                      [Draw,Discard(Hearts,Jack)], 42); false)
           handle IllegalMove => true)
     )
);

test("must end game if the score is higher than the goal",
     assert_true(
         officiate([(Hearts, Jack), (Spades, Num(10)), (Spades, Ace)],
                  [Draw, Draw, Draw, Draw], 11) = 27
     )
    );

run();

(* Homework2 Simple Test *)
use "testing.sml";
open SmlTests;

use "../hw2.sml";

test("give some of nil if there " ,
     assert_true(all_except_option("string", ["string"]) = SOME [])
    );

test("give some of the list with the element removed",
     assert_true(all_except_option("val", ["val", "otherval"]) = SOME ["otherval"])
    );

test("give some of the list with the element removed 2",
     assert_true(all_except_option("val", ["fucked val", "val", "otherval"]) = SOME ["fucked val", "otherval"])
    );

test("test2  is right",
     assert_true(get_substitutions1([["foo"],["there"]], "foo") = [])
    );

test("Example given in the exercise", 
     assert_true(get_substitutions1(
                [["Freddie", "Frederick"], ["Bettie", "Beth"], ["Joshua", "Freddie"]], "Freddie") = ["Frederick", "Joshua"])
    );
(*
val test3 = get_substitutions2([["foo"],["there"]], "foo") = []
test("test3  is right", assert_true(test3))             

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
test("test4  is right", assert_true(test4))             

val test5 = card_color((Clubs, Num 2)) = Black
test("test5  is right", assert_true(test5))             

val test6 = card_value((Clubs, Num 2)) = 2
test("test6  is right", assert_true(test6))             

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
test("test7  is right", assert_true(test7))             

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
test("test8  is right", assert_true(test8))             

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
test("test9  is right", assert_true(test9))             

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
test("test10 is right", assert_true(test10))             

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
test("test11 is right", assert_true(test11))             

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3
test("test12 is right", assert_true(test12))             

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
test("test13 is right", assert_true(test13))             
*)

             
run();
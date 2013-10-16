(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except (s1, stringList) = 
    case stringList of
        [] => []
      | head :: tail => if head = s1
                        then all_except (s1, tail) 
                        else head :: all_except (s1, tail)

fun all_except_option (s1, stringList) = 
    let
        val newList = all_except (s1, stringList);
    in
        case newList of
            [] => SOME []
          | head::tail => if (head::tail) = stringList 
                          then NONE 
                          else SOME (head::tail)
    end
        
fun get_substitutions1 (substitutions, s1) = 
    case substitutions of
        [] => []
      | subList :: rest => 
        case all_except_option (s1, subList) of
            NONE => []
          | SOME aList => aList @ get_substitutions1 (rest, s1)
                                                     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

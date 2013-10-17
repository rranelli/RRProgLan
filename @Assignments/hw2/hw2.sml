(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*problem 1.a*)
fun all_except (_, []) = []
  | all_except (s1, head :: tail) =
    if head = s1
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

(*problem 1.b*)
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (subList :: rest, s1) =
    case all_except_option (s1, subList) of
        NONE => [] @ get_substitutions1 (rest, s1)
      | SOME aList => aList @ get_substitutions1 (rest, s1)

(*problem 1.c*)
fun get_substitutions2 ([], _) = []
  | get_substitutions2 (stringList, s1) =
    let
        fun tailrecursive (acc, []) = acc
          | tailrecursive (acc, subList :: rest) =
            case all_except_option (s1, subList) of
                NONE => tailrecursive (acc, rest)
              | SOME aList => tailrecursive (acc @ aList, rest)
    in
        tailrecursive ([], stringList)
    end

(*problem 1.d*)
(* TODO : DO IT !*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(*problem 2.a*)
fun card_color (Hearts, _) = Red
  | card_color (Diamonds, _) = Red
  | card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black

(*problem 2.b*)
fun card_value (_, Num value) = value
  | card_value (_, Jack) = 10
  | card_value (_, Queen) = 10
  | card_value (_, King) = 10
  | card_value (_, Ace) = 11

(*problem 2.c*)
fun remove_card (cards, card, excpt) =
    case all_except_option (card, cards) of
        NONE => raise excpt
      | SOME remCards => remCards

(*problem 2.d*)
fun all_same_color ([]) = true
  | all_same_color (lastCard :: []) = true
  | all_same_color (firstCard :: secondCard :: rest) =
    card_color (firstCard) = card_color (secondCard)
    andalso
    all_same_color (secondCard :: rest)

(*problem 2.e*)
fun foldLeft (aFunction, aList, initVal) =
    let
        fun foldLeftTailRec (acc, []) = acc
          | foldLeftTailRec (acc, head :: tail) =
            foldLeftTailRec (aFunction(acc, head), tail)
    in
        foldLeftTailRec (initVal, aList)
    end

fun sum_cards (cards) =
    foldLeft (fn (acc,x) => acc + card_value(x), cards, 0)
             
(*problem 2.f*)
fun score (cards, goal) =
    let
        val rawScore = sum_cards (cards)
        val prelimScore = if rawScore > goal
                          then 3 * (rawScore - goal)
                          else (goal - rawScore)
    in
        if all_same_color (cards)
        then prelimScore div 2
        else prelimScore
    end

(*problem 2.g*)
fun officiate (cards, moves, goal) =
    let
        fun playGame (heldCards, [], _) = score (heldCards, goal)
          | playGame (heldCards, _, []) = score (heldCards, goal)

          | playGame (heldCards, nextCard :: remCards, Draw :: remMoves) =
            playGame (nextCard :: heldCards, remCards, remMoves)

          | playGame (heldCards, cards, (Discard aCard) :: remMoves) =
            let val newHand = remove_card (heldCards, aCard,
                                           IllegalMove)
            in
                playGame (newHand, cards, remMoves)
            end
    in
        playGame ([], cards, moves)
    end

(*problem 3.a*)
fun score_challenge (cards, goal) =
    let
        val normalScore = score (cards, goal)
                                
        fun new_card_value (_, Ace) = 1
          | new_card_value (aSuit, aRank) = card_value (aSuit, aRank)
  
        val newRawScore =
            foldLeft (fn (acc, x) => acc + new_card_value (x),
                      cards, 0)
                     
        val newPrelimScore = if newRawScore > goal
                             then 3 * (newRawScore - goal)
                             else (goal - newRawScore)
                                      
        val newScore = if all_same_color (cards)
                       then newPrelimScore div 2
                       else newPrelimScore
    in
        if normalScore < newScore
        then normalScore 
        else newScore
    end

fun officiate_challenge (cards, moves, goal) = 
    let
        fun playGame (heldCards, [], _) = score_challenge (heldCards, goal)
          | playGame (heldCards, _, []) = score_challenge (heldCards, goal)

          | playGame (heldCards, nextCard :: remCards, Draw :: remMoves) =
            playGame (nextCard :: heldCards, remCards, remMoves)

          | playGame (heldCards, cards, (Discard aCard) :: remMoves) =
            let val newHand = remove_card (heldCards, aCard,
                                           IllegalMove)
            in
                playGame (newHand, cards, remMoves)
            end
    in
        playGame ([], cards, moves)
    end

(*problem 3.b*)


(* Coursera Programming Languages, Homework 3, Provided Code *)
exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(*================================================*)
(*============ Here comes my solution ============*)

(* problem 1*)
fun only_capitals (stringList) = 
    List.filter (fn x => Char.isUpper(String.sub (x,0))) stringList

(* problem 2*)
fun longest_string1 [] = ""
  | longest_string1 (head :: tail) = 
    let 
        val selector = fn (x,acc) => 
                          if String.size (acc) >= String.size (x)
                          then acc
                          else x
    in
        List.foldl selector "" (head :: tail)
    end

(* problem 3*)
fun longest_string2 [] = ""
  | longest_string2 (head :: tail) = 
    let 
        val selector = fn (x,acc) => 
                          if String.size (acc) > String.size (x)
                          then acc
                          else x
    in
        List.foldl selector "" (head :: tail)
    end

(* problem 4 *) (* true if first is bigger*)
fun longest_string_helper _ [] = ""
  | longest_string_helper pickFirst (head::tail)  = 
    let
        val selector = 
         fn (x, acc) => 
            if pickFirst (String.size(x), String.size(acc))
            then x
            else acc
    in 
        List.foldl selector "" (head :: tail)
    end

val longest_string3  = 
    longest_string_helper (fn (frst, scnd) => frst > scnd)

val longest_string4  = 
    longest_string_helper (fn (frst , scnd) => frst >= scnd)

(* problem 5 *)
fun longest_capitalized (stringList) =
    (longest_string1 o only_capitals) stringList

(* problem 6 *)
fun rev_string (aString) =
    (String.implode o List.rev o String.explode) aString

(* problem 7 *)
fun first_answer _ [] = raise NoAnswer
  | first_answer getAnswer (question::questions') = 
    case getAnswer question of
        NONE => first_answer getAnswer questions'
      | SOME answer => answer
                              
(* problem 8 *)
fun all_answers _ [] = SOME []
  | all_answers getAnswers (questionList) = 
    let 
        fun all_answers_acc (answersAcc, []) = answersAcc

          | all_answers_acc (answersAcc, question :: questions') = 
            case getAnswers question of
                NONE => all_answers_acc (answersAcc, questions')
              | SOME answers =>
                all_answers_acc (answersAcc @ answers, questions')

    in
        case all_answers_acc ([], questionList) of
            [] => NONE
          | answer::answers => SOME (answer :: answers)
    end

(* problem 9 *)
fun count_wildcards aPattern = 
    g (fn () => 1) (fn x => 0) aPattern

fun count_wild_and_variable_lengths aPattern = 
    g (fn() => 1) (fn x => String.size(x)) aPattern

fun count_some_var (aString, aPattern) = 
    g (fn() => 0) (fn x => if x = aString then 1 else 0) aPattern

(* problem 10 *)
fun check_pat aPattern = 
    let
        fun getVarNames p =
	        case p of
	            Variable x        => [x]
	          | TupleP ps         => 
                    List.foldl (fn (p,acc) =>
                                   (getVarNames p) @ acc) [] ps
	          | _                 => []

        fun allUnique [] = true
          | allUnique (head :: tail) = 
            not (List.exists (fn x => head = x) tail)
            andalso allUnique (tail)
    in
        allUnique (getVarNames (aPattern))
    end

(* problem 11 *)
fun match (_, Wildcard) = SOME []
  | match (Unit, UnitP) = SOME []

  | match (Const(val1), ConstP(val2)) = 
    if val1 = val2 
    then SOME [] 
    else NONE

  | match (aValue, (Variable aVar)) =
    SOME [(aVar, aValue)]

  | match ((Constructor (s1, aVal)), (ConstructorP (s2, aPat))) =
    if s1 = s2
    then match (aVal, aPat)
    else NONE

  | match ((Tuple vals),(TupleP pats)) = 
        all_answers match (ListPair.zip (vals, pats))

  | match (_, _) = NONE
        
(* problem 12 *)
fun first_match aVal pats =
    SOME (first_answer (fn pat => match (aVal, pat)) pats)
    handle NoAnswer => NONE

(* challenge problem! *)

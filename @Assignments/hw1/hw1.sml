(* Homework 1 - Renan Ranelli*)

(*problem 1*)
fun is_older (a, b) =
    let
        val (y1,m1,d1) = a;
        val (y2,m2,d2) = b;
    in
        if y1 < y2 then true else
        if y1 > y2 then false else
        if m1 < m2 then true else
        if m2 > m2 then false else
        if d1 < d2 then true else false
    end

(*problem 2*)
fun number_in_month ([] , _)  = 0
  | number_in_month ((x,y,z)::xs , month) =  if y = month then
                                                 1 + number_in_month (xs, month)
                                             else number_in_month (xs, month)

(*problem 3*)
fun number_in_months ([], _) = 0
  | number_in_months (_, []) = 0
  | number_in_months (dates, month::months') =
    number_in_month(dates, month) + number_in_months(dates, months')

(*problem 4*)
fun dates_in_month ([], _) = []
  | dates_in_month (datelist, month) =
    case datelist of
        [] => []
      | date :: dates' =>
	let
            val (thisYear, thisMonth, thisDay) = date
	in
            if thisMonth = month then (thisYear, thisMonth, thisDay) ::
                                      dates_in_month(dates', month)
            else dates_in_month (dates', month)
	end

(*problem 5*)
fun dates_in_months (_,[]) = []
  | dates_in_months (dates, month :: months') =
    dates_in_month  (dates, month) @ dates_in_months (dates, months')

(*problem 6*)
fun get_nth (head::tail, n) = if n = 1 then head else get_nth(tail, n-1)

(*problem 7*)
fun date_to_string ((year, month, day)) =
    let
        val monthsAsStrings = ["January", "February", "March", "April",
                               "May", "June", "July", "August", "September",
                               "October", "November", "December"]
    in
        get_nth(monthsAsStrings, month) ^ " " ^
        Int.toString(day) ^ ", " ^
        Int.toString(year)
    end

(*problem 8*)
fun acchelper (sum, number::numbers', acc, n) =
    if acc + number >= sum then n
    else acchelper(sum, numbers', acc + number, n+1)

fun number_before_reaching_sum (sum, numberList) =
    acchelper (sum, numberList, 0, 0)

(*problem 9*)
fun what_month (days) =
    let
        val daysInMonths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(days, daysInMonths) + 1
    end

(*problem 10*)
fun month_range (day1, day2) =
    if (day1 > day2) then [] else
    what_month(day1) :: month_range(day1+1, day2)

(*problem 11*)
fun hhmin (_, head::[]) = head
  | hhmin (comparer, head::tail) =  (* comparer(a,b) returns true if a<b*)
    let
        val minOfTail = hhmin (comparer, tail)
    in
        if comparer (head, minOfTail) then head
        else minOfTail
    end

fun oldest ([]) = NONE
  | oldest (dates) = SOME (hhmin (is_older, dates))

(*problem 12 - Challenge 1*)
fun contains (_, []) = false
  | contains (value, thisValue::values') =
    if value = thisValue then true else contains(value, values')

fun unique ([]) = []
 |  unique (value::values') =
    if contains(value, values') then unique(values')
    else value :: unique(values')

fun number_in_months_challenge ([], _) = 0
  | number_in_months_challenge (_, []) = 0
  | number_in_months_challenge (dates, months) =
    number_in_months (dates, unique(months))

fun dates_in_months_challenge (_, []) = []
  | dates_in_months_challenge (dates, months) =
    dates_in_months (dates, unique(months))

(*problem 13 - Challenge 2*)
fun reasonable_date ((year,month,day)) =
    let
        val isLeap   = (year mod 4 = 0) andalso (year mod 100 <> 0)
        val days     = [31,28,31,30,31,30,31,31,30,31,30,31]
        val leapDays = [31,29,31,30,31,30,31,31,30,
                        31,30,31]
        val daysInMonths = if isLeap then leapDays else days
    in
        if (year <> 0) andalso
           (month > 0) andalso
           (month <= 12) andalso
           (day > 0) andalso
           (day <= get_nth(daysInMonths, month))
        then
            true
        else
            false
    end

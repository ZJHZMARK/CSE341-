(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml";

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")


(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

(*takes a int i and return a list containing starting from i to 0.0*)
fun make_silly_json (i:int)=
    let
	fun eval 0=[]
	  | eval i' =Object[("n",Num(int_to_real i')),("b",True)]::eval(i'-1)
    in
	Array(eval i)
    end

(*take a key and list and return a option type of that value, if not found return NONE*)	
fun assoc(k, xs)=
    case xs of
	[] => NONE
      | (k',v1)::xs' => if k=k' then SOME v1 else assoc(k,xs')


(*get a designate value if the given value is in its field*)						       
fun dot(j,f)=
    case j of
	Object x => assoc(f,x)
      | _ => NONE

(*return a list if the passed in argument is Json type and an empty list otherwise *)
fun one_fields(j)=
   
	let fun onefields_helper(j,list) =
		 case j of
		     (x,y)::xs => onefields_helper(xs, x::list)
		   | [] => list
			     
        in
	    case j of
		Object j => onefields_helper(j,[])
	      | _ => []
			 
	
        end
	 
(*return true if the given string list contains no duplicate, false otherwise*)
fun no_repeats (str)= length(dedup str)=length str


					       
(*return true if the given Json object has no duplicate field name, false otherwise*)
fun recursive_no_field_repeats(j) =
    
    case j of
	Array arr =>
	let fun arrHelper a =
		case a of
		    [] => true
		  | x::xs'tl => (recursive_no_field_repeats(x) andalso
				 recursive_no_field_repeats(Array xs'tl))
	in
	    arrHelper(arr)
	end

	    
      | Object obj =>
	let fun helper1 (obj) =
		let fun innerHelper(j, acc) =
			case j of [] => recursive_no_field_repeats(Array(acc))
				| (x, y)::xs => innerHelper(xs, y::acc)
		in
		    innerHelper(obj, [])
		end 		  
	    in
		no_repeats(one_fields(Object obj)) andalso
		helper1(obj)
	    end

				    
       | _  => true
			 
				    
					 
	  	    
				    
	
	    

	
(*raise exn if the given list is not sorted and return a list which contains the value and its occurance*)	
fun count_occurrences(str, e)=
    let
	fun countHelper(rest, currentStr, currentCount, answerList) =
	    case rest of
		[] =>  (currentStr, currentCount)::answerList
	     |  x::xs =>
		(case strcmp(currentStr,x) of 
			   EQUAL => countHelper(xs,x,currentCount+1,answerList )
			 | LESS => countHelper(xs, x, 1, (currentStr,currentCount)::answerList)
			 | GREATER => raise e)

    in
	case str of
	    [] => []
	  | x::xs => countHelper(xs,x,1,[])
    end

  

	
(*select any object in the json list that has field equal to the string and return those in a list *)
fun string_values_for_field(str,j)=
    case j of
	[] => [] 
      | x::xs =>
	case dot(x, str) of
	    SOME(String s) => s::string_values_for_field(str,xs)
	   |_ => string_values_for_field(str,xs)
	   
	 
				     					       
(* histogram and historgram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

     
fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))


(**** PUT PROBLEMS 9-11 HERE ****)

(*returning a subset of passed in argument j with field
 name equal to the first string passed in
and the field content equal
 to the second passed in string*)
fun filter_field_value(str1,str2,j)=
    case j of
	[]=>[]
      | x::xs =>
	case dot(x,str1) of
	    SOME(String str3)=>
	    if str3=str2
	    then x::filter_field_value(str1,str2,xs)
	    else
		filter_field_value(str1,str2,xs)
	  | _ =>filter_field_value(str1,str2,xs)


			 
(*binding a histogram to the variable with given string and list*)			  
val large_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description", large_incident_reports_list);

(*binding a histogram to the variable with given string and list*)
val large_hundred_block_location_histogram =
    histogram_for_field("hundred_block_location", large_incident_reports_list);




			  
;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

(*binding a json list with given string1 and string2 and list*)
val forty_third_and_the_ave_reports =
   filter_field_value("hundred_block_location",
		      "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list);

(*binding a json list with given String1 and given list*)
val forty_third_and_the_ave_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports);
		       
(*binding a json list with given String1 and String2 and given list *)
val nineteenth_and_forty_fifth_reports =
    filter_field_value("hundred_block_location",
		       "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list);
(*binding a histogram with given string and the given list*)
val nineteenth_and_forty_fifth_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports)



;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)

(*take a sepearator and a string list and returning a string with the 
string in the list and seperrator concat together*)
fun concat_with(separator, str)=
    case str of
	 x::[] => x
		  
      | x::xs => (x ^ separator) ^ (concat_with(separator, xs))
      
      | [] => ""
					


(*Return a quoted string depending on the given string*)		     
fun quote_string(str) =
    "\"" ^ str ^ "\""


(*return a string by taking a real as argument*)		     
fun real_to_string_for_json(rea) =
    if rea >= 0.0
    then
	real_to_string(rea)
    else
	"-" ^ real_to_string(real_abs(rea))


(*take a json onject and convert it to string type*)
fun json_to_string(j) =
    let fun objHelper(a) =
	    ( case a of
		  [] => ""
		   | (x,y)::[]=>  quote_string(x) ^ " : " ^  json_to_string(y)
 
		   | ((x,y)::xs') => quote_string(x) ^ " : " ^  json_to_string(y) ^ ", " ^ objHelper(xs')
												 
	       )
         fun arrHelper(a) =
	   ( case a of
		[] => ""
	       | x::[] =>  json_to_string(x)			 
	       | x::xs => json_to_string(x) ^ ", " ^ arrHelper(xs))
    in
	case j of
	    True => "true"
	  | False => "false"
	  | Null => "null"
	  | String str => quote_string(str)
	  | Num a => real_to_string_for_json(a)
	  | Object obj => "{" ^ objHelper(obj) ^ "}"
	  | Array arr => "[" ^ arrHelper(arr) ^ "]"
    end
	
							   
 


(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)


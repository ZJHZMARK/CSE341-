

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list


			      
(* (answer for the 9 a)function g takes function f1 and funtionf2 
and a pattern as argument in which 
function1 deal with WildcardP and function2 deal with string *)			      
fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

				
(*take a string list and filter out a string list with string only has string start
 with lowercase string *)
fun only_lowercase str =
    List.filter(fn (s) => Char.isLower(String.sub(s, 0))) str
	       
(*take a string list and return the longest string in the string list which is closer
to the beginning of the list*)
fun longest_string1 str =
    foldl (fn (str1, str2) => if String.size(str1) > String.size(str2) then str1 else str2) "" str

(*take a string list and return the longest string in the string
 list which is closer to the endind
part of the string list*)
fun longest_string2 str =
    foldl (fn (str1, str2) => if String.size(str1) >= String .size(str2) then str1 else str2) "" str

(*take a function and string list as argument and fiter out a string list which a single argument would
pass the function test.*)
fun longest_string_helper f str =
    foldl (fn (str1, str2) => if f(String.size(str1), String.size(str2)) then str1 else str2) "" str

(*take a a string list and return the longest string which is closer to 
the beggining of the string list*)
val longest_string3 = longest_string_helper(fn(str1, str2) => str1 > str2)

(*take a string list and return the longest string which is closer to 
the ending part of the string list*)					   
val longest_string4 = longest_string_helper(fn(str1, str2) => str1 >= str2)


(*take a string list and return the longest string which starts with a 
lowercase letter*)					   
val longest_lowercase = longest_string1 o only_lowercase


(*take a random string and return a string which filter out letter 'x' or 'X' 
and all rest letter capitalized*)					      
val caps_no_X_string  =
    String.implode
    o foldr(fn (x, result) => Char.toUpper(x) :: result) []
    o List.filter(fn x=> x <> #"x" andalso x <> #"X")
    o String.explode

(*take a function f and list xs and return the first element that 
pass the test in f*)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs'=> (case f x of
		      NONE => first_answer f xs'
		    | SOME y => y)

(*take a funtion f and list xs and return all the elements in the
list that pass the f test in list form*)
fun all_answers f xs =
    let fun helper [] result = SOME result
	  | helper (x::xs') result = (case f x of
				       NONE => NONE
				       | SOME y => helper xs' (result@y))

    in
	helper xs []
    end

(*takes a patern as argument and return how many wildcard is in the pattern*)
val count_wildcards = g (fn() => 1) (fn str => 0)

			
(*take a pattern and return the number of wildcards and sum of the 
string lengths of all the variables in the varialble patterns
it contains. *)
val count_wild_and_variable_lengths = g (fn() => 1) (fn str => String.size str)

(*take a stirng and a pattern and return the times of the 
string appear in the pattern as variable*)
fun count_a_var (str, p) =
    g (fn() => 0) (fn str2 => if str=str2 then 1 else 0) p


(*take a pattern as argument and return true if and only if all the variables
appearing in the pattern are distinct from each other.*)
fun check_pat p =
    let
	fun getList p result =
	    case p of
		VariableP x => x::result
	      | TupleP ps => foldl(fn (p, result) => getList p result) result ps
	      | ConstructorP(_, p)  => getList p result
	      | _ => result
	fun uniqueHelper str =
	    case str of
		[] => true
	      | x::xs' => if List.exists(fn str2 => str2 = x) xs'
			  then false
			  else
			      uniqueHelper xs'

    in
	uniqueHelper(getList p [])
		    
    end


(*take a valu*pattern and return a (string*valu) list option which the pattern
matches in the passed in argument.*)
fun match vapa =
    case vapa of
	(_, WildcardP) => SOME[]
      | (v, VariableP s)  => SOME[(s,v)]
      | (Unit, UnitP) => SOME[]
      | (Constant a,ConstantP b) => (if a = b
				    then SOME[]
				    else
					NONE)
      | (Constructor (s1, v), ConstructorP (s2, p)) => (if s1 = s2
						       then match (v, p)
						       else
							   NONE)
      | (Tuple (vl), TupleP (pl)) => (if (length vl) = (length pl)
				 then
				     all_answers match(ListPair.zip(vl, pl))
				 else
				     NONE)
      | _ => NONE


(*take a value and a list of patterns and reuturn a (string*valu) list option if
the 1st is the list of bindings for the first pattern in the list that matches 
else return NONE.*)
fun first_match n pList =
    SOME(first_answer (fn pa => match (n, pa)) pList)
    handle NoAnswer => NONE

			   

			   


			   
		 

    

    
    

	
	
			      
					

		
	

		     




		     

		     
				     
		       
							    
    						    
					       
    
					   

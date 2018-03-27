use "hw3.sml";

val testList1 = ["Apple", "amazon", "Microsoft", "nvdia", "facebOOK","abcdefghijklmXXX"];
		    
val testList2 = ["apple", "amazon", "microsoft", "nvbia", "facebook", "aXXXbcdefghijklm"];

val p1 = TupleP [WildcardP, VariableP "apple",UnitP,ConstantP 10,ConstructorP ("bbbbb",VariableP "aaaaa")];

val p2 = TupleP [VariableP "apple",ConstructorP ("bbbbb",VariableP "aaaaaaaa")];

val p3 = WildcardP;

val p4 = TupleP [WildcardP,TupleP [WildcardP,TupleP [WildcardP,UnitP],ConstantP 10],ConstructorP ("aaaaaaaaa",WildcardP)];

val p5 = TupleP [VariableP "aaaaaaa", VariableP "bbbbbbbbb"];

val p6 = TupleP [VariableP "aaaaaaa", VariableP "bbbbbbbbb", VariableP "wswswswswswsws"];

val p7 = WildcardP;

val p8 = VariableP "aaaaaaaa";

val p9 = UnitP;

val p10 = ConstantP 100;

val p11 = TupleP [UnitP];

val p12 = ConstructorP("abc", TupleP[UnitP, VariableP "aaaaa", ConstantP 100]);
		 
val v1 = Constructor("foo", Constant 100);

val v2 = Tuple[Unit, Constant 100];

val v3 = Unit;

val v4 = Constant 100;

val v5 = Tuple [Unit, Constant 100];

val v6 = Constructor("abc", Tuple [Unit, Constant 0, Constant 100]);

val pList  =[p1, p2, p3, p4, p5, p6];

val pList2 = [p7, p8, p9, p10, p11, p12];

		
val testString1 = "aBxXXxDdx";
val testString2 = "xxxxxxxxxxxabcd";

fun checkHelper1 p = if check_pat p then SOME[p] else NONE;
fun checkHelper2 p = if check_pat p then SOME p else NONE;



		
		    

val test1 = only_lowercase testList1;
val test2 = only_lowercase testList2;

val test3 = longest_string1 testList1;
val test4 = longest_string1 testList2;

val test5 = longest_string2 testList1;
val test6 = longest_string2 testList2;


			    
val test7 = longest_string3 testList1;
val test8 = longest_string3 testList2;

val test9 = longest_string4 testList1;
val test10 = longest_string4 testList2;

val test11 = longest_lowercase testList1;
val test12 = longest_lowercase testList2;


val test13 = caps_no_X_string testString1;
val test14 = caps_no_X_string testString2;


val test15 = count_wildcards p1;
val test16  = count_wildcards p3;
val test17  = count_wildcards p4;

val test18 = count_wild_and_variable_lengths p1;
val test19 = count_wild_and_variable_lengths p3;
val test20 = count_wild_and_variable_lengths p4;

val test21 = count_a_var ("bbbbb", p1);
val test22 = count_a_var ("aaaaaaaa", p2);
val test23 = count_a_var ("aaaaaaaaa",p4);

val test24 = check_pat p1;
val test25 = check_pat p4;
val test26 = check_pat p2;

val test27 = first_answer checkHelper2 pList;
val test28 = all_answers checkHelper1 pList;

val test29 = match(v1, p7);
val test30 = match(v2, p7);
val test31 = match(v3, p9);
val test32 = match(v4, p10);
val test33 = match(v5, p11);

val test34 = first_match v6 pList2;


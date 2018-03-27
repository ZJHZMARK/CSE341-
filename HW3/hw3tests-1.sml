(* CSE 341, Homework 3 Tests *)

use "hw3.sml";

val stringList1 = ["You", "don't", "Know", "The", "power", "of", "the", "Dark", "side"];
val stringList2 = ["a", "Lannister", "always", "pays", "his", "Debts"];
val emptyList = [];

val test11 = only_lowercase stringList1 = ["don't", "power", "of", "the", "side"];
val test12 = only_lowercase stringList2 = ["a", "always", "pays", "his"];
val test21 = longest_string1 stringList1 = "don't";
val test22 = longest_string1 stringList2 = "Lannister";
val test23 = longest_string1 emptyList = "";
val test31 = longest_string2 stringList1 = "power";
val test32 = longest_string2 stringList2 = "Lannister";
val test41 = longest_string_helper (fn(x, curr) => x = 4) stringList1 = "side";
val test42 = longest_string_helper (fn(x, curr) => x = 3) stringList1 = "the";
val test43 = longest_string3 stringList1 = "don't";
val test44 = longest_string3 stringList2 = "Lannister";
val test45 = longest_string3 emptyList = "";
val test46 = longest_string4 stringList1 = "power";
val test47 = longest_string4 stringList2 = "Lannister";
val test48 = longest_string4 (stringList1@stringList2) = "Lannister";
val test51 = longest_lowercase stringList1 = "don't";
val test52 = longest_lowercase stringList2 = "always";
val test53 = longest_lowercase (stringList1@stringList2) = "always";
val test61 = caps_no_X_string "abcDEXXFgawWxax" = "ABCDEFGAWWA";
val test62 = caps_no_X_string "xxxxXXXX" = "";
val test63 = caps_no_X_string "xaxBxcXdxE" = "ABCDE";

val wild1 = WildcardP;
val wild2 = WildcardP;
val wild3 = WildcardP;
val var1 = VariableP("yes");
val var2 = VariableP("no");
val var3 = VariableP("Lannister");
val uni1 = UnitP;
val uni2 = UnitP;
val Con1 = ConstantP(1);
val Con2 = ConstantP(100);
val Const1 = ConstructorP("yes", wild1);
val Const2 = ConstructorP("no", var1);
val Tuple1 = TupleP([wild2, var2, var2, var1, uni1, Con1, Const1, Const2]);
val Const3 = ConstructorP("University", Tuple1);
val Tuple2 = TupleP([wild3, var3, var3, var1, var2, uni2, Con2, Const3]);
val Tuple3 = TupleP([Const2, var2, TupleP([var3, uni2])]);

val test91 = count_wildcards Tuple2 = 3;
val test92 = count_wildcards Tuple1 = 2;
val test93 = count_wildcards Const1 = 1;
val test94 = count_wildcards Const2 = 0;
val test95 = count_wildcards var1 = 0;
val test96 = count_wild_and_variable_lengths Tuple2 = 36;
val test97 = count_wild_and_variable_lengths Tuple1 = 12;
val test98 = count_wild_and_variable_lengths Const1 = 1;
val test99 = count_wild_and_variable_lengths Const2 = 3;
val test910 = count_wild_and_variable_lengths Con1 = 0;
val test911 = count_a_var ("yes", Tuple2) = 3;
val test912 = count_a_var ("yes", Tuple1) = 2;
val test913 = count_a_var ("no", Tuple2) = 3;
val test914 = count_a_var ("Lannister", Tuple2) = 2;
val test914 = count_a_var ("Lannister", Tuple1) = 0;
val test914 = count_a_var ("DeepDarkFantasy", Tuple2) = 0;
val test101 = check_pat Tuple3;
val test102 = not(check_pat Tuple2);
val test103 = not(check_pat Tuple1);
val test104 = check_pat Const2;

val valCon1 = Constant(1);
val valCon2 = Constant(2);
val valUni1 = Unit;
val valConst1 = Constructor("yes", Constant(3));
val valConst2 = Constructor("no", valUni1);
val valTuple1 = Tuple([valConst1, valCon1, valUni1, valCon2, valUni1, valCon1, valConst1, valConst2]);
val FinalTestTupleP = [Tuple1, Con1, Con2, var1, var2];

val test111 = match (valTuple1, Tuple1) = SOME [("no", valCon1),("no", valUni1),("yes", valCon2),("yes", valUni1)];
val test112 = match (valConst2, Const1) = NONE;
val test113 = match (valConst2, Const2) = SOME[("yes", valUni1)];
val test114 = match (valTuple1, Tuple2) = NONE;
val test121 = first_match valTuple1 FinalTestTupleP = SOME [("no", valCon1),("no", valUni1),("yes", valCon2),("yes", valUni1)];
val test122 = first_match valCon1 FinalTestTupleP = SOME [];
val test123 = first_match valConst2 FinalTestTupleP = SOME [("yes", valConst2)];

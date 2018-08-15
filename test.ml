
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result:
     - the MOCaml declaration is a string of exactly what you would type into the interpreter prompt,
       without the trailing ";;"
     - the expected result is a string of exactly what you expect the interpreter to print as a result
   use the string "dynamic type error" as the result if a DynamicTypeError is expected to be raised.
   use the string "match failure" as the result if a MatchFailure is expected to be raised.
   use the string "implement me" as the result if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
    (* YOU NEED TO ADD A LOT MORE TESTS! *)
		("3", "3"); 
    ("false", "false");
    ("let x = 34", "val x = 34");
    ("y", "dynamic type error");
    ("let y = true", "val y = true");
    ("-34", "-34");
    ("-x", "-34");
    ("-y", "dynamic type error");
    ("let z = 4", "val z = 4");

    ("x+z", "38");
    ("(-34)+(-4)", "-38");
    ("(-x)+(-z)", "-38");
    ("(-(-34))+(-(-4))", "38");
    ("(-x)+(-4)", "-38");
    ("(-4)+(-x)", "-38");
    ("(-x)+(-(-4))", "-30");
    ("(-(-4))+(-x)", "-30");
    ("(-4)+(-(-34))", "30");
    ("(-(-34))+(-4)", "30");

    ("x-z", "30");
    ("(-34)-(-4)", "-30");
    ("(-x)-(-z)", "-30");
    ("(-(-34))-(-(-4))", "30");
    ("(-x)-(-4)", "-30");
    ("(-4)-(-x)", "30");
    ("(-x)-(-(-4))", "-38");
    ("(-(-4))-(-x)", "38");
    ("(-4)-(-(-34))", "-38");
    ("(-(-34))-(-4)", "38");

    ("x*z", "136");
    ("(-34)*(-4)", "136");
    ("(-x)*(-z)", "136");
    ("(-(-34))*(-(-4))", "136");
    ("(-x)*(-4)", "136");
    ("(-4)*(-x)", "136");
    ("(-x)*(-(-4))", "-136");
    ("(-(-4))*(-x)", "-136");
    ("(-4)*(-(-34))", "-136");
    ("(-(-34))*(-4)", "-136");

    ("x + 4", "38");

    ("x>z", "true");
    ("z>x", "false");
    ("x=34", "true");
    ("z=4", "true");
    ("x=4", "false");
    ("z=34", "false");
    ("(-x) = -34", "true");
    ("(-x) = (-x)", "true");
    ("x=z", "false");
    ("x = -(-34)", "true");
    ("34 > 4", "true");
    ("4 > 34", "false");
    ("z > 4", "false");
    ("x > -4", "true");

    ("let w = false", "val w = false");
    ("if y then 5 else 4", "5");
    ("if w then 5 else 4", "4");
    ("if x=34 then 5+4 else 5-4", "9");
    ("if x=4 then 5+4 else 5-4", "1");
    ("if z=4 then 5*4 else z", "20");
    ("if z=34 then x else y", "true");

    ("(x+z) + (1+2)", "41");
    ("(1+2) + (x+z)", "41");

    ("(1,2,3)", "(1, 2, 3)");
    ("(1, z, 2)", "(1, 4, 2)");
    ("(x,y,z,w)", "(34, true, 4, false)");
    ("Leaf", "Leaf");
    ("Leaf 1", "Leaf 1");
    ("Node (true,1,Leaf)", "Node (true, 1, Leaf)");

    ("let f = function x -> x + 2", "val f = <fun>");
    ("f 1", "3");
    ("f 10", "12");
    ("f (-2)", "0");

    ("let m = function x -> 
        match x with 
          2 -> x
        | 0 -> true", "val m = <fun>");
    ("m 2", "2");
    ("m 0", "true");
    ("m 3", "match failure");

		]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError _ -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  

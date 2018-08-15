(* Name: Andy Kuang

   UID: 704483703

   Others With Whom I Discussed Things: None

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(s), value) -> Env.add_binding s value (Env.empty_env())
    | (TuplePat(lst), TupleVal(lst2)) ->
        (try 
          (List.fold_right2 
          (fun elem elem2 restOfList -> Env.combine_envs restOfList (patMatch elem elem2))
          lst lst2 (Env.empty_env())
          )
      with
        Invalid_argument "List.fold_right2" -> raise MatchFailure
      )

    | (DataPat(str, opt), DataVal (str2, opt2)) ->
        (if str = str2 then
          (match (opt, opt2) with 
            (None, None) -> Env.empty_env()
          | (Some val1, Some val2) -> Env.combine_envs (patMatch val1 val2) (Env.empty_env())
          | _ -> raise MatchFailure
          )
        else
          raise MatchFailure
        )
    | _ -> raise MatchFailure


(* unused functions

let rec parseInputInt (x:moexpr) (env:moenv) : moexpr =
    match x with
    IntConst(i) -> IntConst(i)
  | Var(s) -> (try 
      (match (Env.lookup s env) with
        IntVal(i) -> IntConst(i)
      | _ -> raise (DynamicTypeError "variable is not an integer")
      )
    with
      Env.NotBound -> raise (DynamicTypeError "variable is not bound")
    )
  | Negate(value) -> 
      (match value with
          IntConst(a) -> IntConst(-a)
      | Var(s) ->
          (try
           (match Env.lookup s env with
           IntVal(i) -> IntConst(-i)
          | _ -> raise (DynamicTypeError "Can only negate integers")
            )
          with
            Env.NotBound -> raise (DynamicTypeError "variable is not bound")
          )
      | Negate(value2) -> value2
      | _ -> raise (DynamicTypeError "Can only negate integers")
      )
  | BinOp(first, oper, second) -> 
      (match (first, oper, second) with
        (IntConst(i), Plus, IntConst(j)) -> IntConst(i+j)
      | (IntConst(i), Minus, IntConst(j)) -> IntConst(i-j)
      | (IntConst(i), Times, IntConst(j)) -> IntConst(i * j)
      | (IntConst(i), Eq, IntConst(j)) -> if i=j then BoolConst(true) else BoolConst(false)
      | (IntConst(i), Gt, IntConst(j)) -> if i>j then BoolConst(true) else BoolConst(false)
      | (_, oper, _) -> parseInputInt (BinOp(parseInputInt first env, oper, parseInputInt second env)) env
      )
  | _ -> raise (DynamicTypeError "Input parsing failed int")


let rec parseInputBool (x:moexpr) (env:moenv) : moexpr =
  match x with
    BoolConst(i) -> BoolConst(i)
  | Var(s) -> (try
           (match Env.lookup s env with
           BoolVal(i) -> BoolConst(i)
          | _ -> raise (DynamicTypeError "variable is not a boolean")
            )
          with
            Env.NotBound -> raise (DynamicTypeError "variable is not bound")
          )
  | BinOp(first, oper, second) ->
      (match (first, oper, second) with
        (IntConst(i), Eq, IntConst(j)) -> if i=j then BoolConst(true) else BoolConst(false)
      | (IntConst(i), Gt, IntConst(j)) -> if i>j then BoolConst(true) else BoolConst(false)
      | (IntConst(i), _, IntConst(j)) -> raise (DynamicTypeError "operator does not produce boolean")
      | (_, oper, _) -> parseInputBool (BinOp(parseInputInt first env, oper, parseInputInt second env)) env
        )
  | _ -> raise (DynamicTypeError "input parsing failed bool")

let rec parseInputFunction (x:moexpr) (env:moenv) : moexpr =
  match x with
    Function(pat, exp) -> Function(pat, exp)
  | Var(s) -> (try
           (match Env.lookup s env with
           FunctionVal(opt, pat, exp, env) -> Function(pat, exp)
          | _ -> raise (DynamicTypeError "variable is not a boolean")
            )
          with
            Env.NotBound -> raise (DynamicTypeError "variable is not bound in parseInputFunction")
          )
  | _ -> raise (DynamicTypeError "input parsing failed function")
*)


let rec movaluetoExp (x:movalue) (env:moenv) : moexpr =
  match x with
    IntVal(i) -> IntConst(i)
  | BoolVal(i) -> BoolConst(i)
  | FunctionVal(opt, pat, exp, env2) -> Function(pat, exp)
  | TupleVal(lst) -> Tuple(List.map (fun x -> movaluetoExp x env) lst)
  | DataVal(str, opt2) ->
      (match opt2 with
        (None) -> Data(str, None)
      | (Some value) -> Data(str, Some (movaluetoExp value env))
    )

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
  | BoolConst(b) ->  BoolVal(b)
  
  | Var(s) -> 
      (try
        Env.lookup s env
      with
        Env.NotBound -> raise (DynamicTypeError "variable is not bound")
      )

  | BinOp(first, oper, second) ->
      (match (first, oper, second) with
        (IntConst(i), Plus, IntConst(j)) -> IntVal(i+j)
      | (IntConst(i), Minus, IntConst(j)) -> IntVal(i-j)
      | (IntConst(i), Times, IntConst(j)) -> IntVal(i * j)
      | (IntConst(i), Eq, IntConst(j)) -> if i=j then BoolVal(true) else BoolVal(false)
      | (IntConst(i), Gt, IntConst(j)) -> if i>j then BoolVal(true) else BoolVal(false)
      | (FunctionCall(_, _), oper, FunctionCall(_, _)) ->
        evalExpr (BinOp((movaluetoExp(evalExpr first env) env), oper, 
                        (movaluetoExp(evalExpr second env) env))) env
      | (FunctionCall(_, _), oper, second) -> 
        evalExpr (BinOp((movaluetoExp(evalExpr first env) env), oper, second)) env
      | (first, oper, FunctionCall(_, _)) -> 
        evalExpr (BinOp(first, oper, (movaluetoExp(evalExpr second env) env))) env
      | (_, oper, _) -> evalExpr (BinOp(movaluetoExp (evalExpr first env) env, oper, movaluetoExp (evalExpr second env) env)) env
      )

  | Negate(value) -> 
      (match value with
          IntConst(a) -> IntVal(-a)
      | Var(s) ->
          (try
           (match Env.lookup s env with
           IntVal(i) -> IntVal(-i)
          | _ -> raise (DynamicTypeError "Can only negate integers"))
          with
            Env.NotBound -> raise (DynamicTypeError "variable is not bound"))
      | Negate(value2) -> evalExpr value2 env
      | _ -> raise (DynamicTypeError "Can only negate integers")
      )

  | If(exp1, exp2, exp3) -> 
      (match exp1 with
        BoolConst(true) -> evalExpr exp2 env
      | BoolConst(false) -> evalExpr exp3 env
      | FunctionCall(exp, exp2) -> evalExpr (If ((movaluetoExp(evalExpr exp1 env) env), exp2, exp3)) env
      | _ -> evalExpr (If (movaluetoExp (evalExpr exp1 env) env, exp2, exp3)) env
      )
  
  | Function (pat, exp) -> FunctionVal (None, pat, exp, env)

  | FunctionCall(exp, exp2) -> 
      (match evalExpr exp env with
        FunctionVal (opt, pat, exp3, env2) -> 
              (match opt with 
              (None) -> evalExpr exp3 (Env.combine_envs env2 (patMatch pat (evalExpr exp2 env)))
            | (Some x) -> evalExpr exp3 (Env.add_binding x (evalExpr exp env) ((patMatch pat (evalExpr exp2 env))))
              )
      | _ -> raise (DynamicTypeError "type error in functioncall")
      )

  | Match (exp, lst) ->
      (match lst with
        [] -> raise MatchFailure
      | (pat,exp2)::t -> 
          try 
              evalExpr exp2 (Env.combine_envs env (patMatch pat (evalExpr exp env)))
          with 
              MatchFailure -> evalExpr (Match(exp, t)) env
      )

  | Tuple(lst) -> TupleVal(List.map (fun x -> (evalExpr x env)) lst)

  | Data(str, opt) -> 
      (match opt with
        None -> DataVal(str, None)
      | Some value -> DataVal(str, Some (evalExpr value env))
      )

  | _ -> raise (ImplementMe "expression evaluation not implemented")


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | Let(x, exp) -> (Some x, evalExpr exp env)
    | LetRec(x, exp) -> 
        (match exp with
          Function(pat, exp) -> (Some x, (FunctionVal(Some x, pat, exp, env)))
        | _ -> raise (DynamicTypeError "the given exp is not a Function")
        )
    | _ -> raise (DynamicTypeError "the expression is not valid")


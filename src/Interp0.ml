open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith env = function
  | Var var -> env var
  | Int int -> int
  | Plus (a, b) -> eval_arith env a + eval_arith env b
  | Times (a, b) -> eval_arith env a * eval_arith env b
;;

let rec eval_bool env = function
  | True -> true
  | False -> false
  | Not exp -> if  eval_bool env exp then false else true
  | And (first, second) -> eval_bool env first && eval_bool env second
  | Or (first, second) -> eval_bool env first || eval_bool env second
  | Bool_op (first, op, second) -> match op with 
                                   | Lt -> eval_arith env first < eval_arith env second
                                   | Gt -> eval_arith env first > eval_arith env second
                                   | Eq -> eval_arith env first = eval_arith env second
;;

let rec eval_statement env = function
  | Skip -> env
  | Assign (var, exp) ->
    let result = eval_arith env exp in
    fun var' -> if eval_bool env (Bool_op (Var var, Eq, Var var')) then result else env var'
  | Seq (first, second) -> let env2 = eval_statement env first in eval_statement env2 second
  | If (cond, true_, false_) -> if eval_bool env cond then eval_statement env true_ else eval_statement env false_
  | While (cond, body) as loop -> if eval_bool env cond then eval_statement env (Seq(body, loop)) else env
;;

let eval_statement =
  eval_statement (fun x -> failwith @@ "Not found: " ^ x)
;;

[@@@ocaml.warning "-27"]

(* 1: CPS *)
open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith (env, arith) return =
  match arith with
  | Var var -> return (env, env var)
  | Int int -> return (env, int)
  | Plus (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int + b_int)))
  | Times (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int * b_int)))
;;

let rec eval_bool (env, bool) return =
  match bool with
  | True -> return(env, true)
  | False -> return(env, false)
  | Not exp -> 
    let return(_, result) = eval_bool(env, exp) return in
    if result then return(env, false) else return(env, true)
  | And (first, second) -> 
    let return(_, result1) = eval_bool(env, first) return in
    let return(_, result2) = eval_bool(env, second) return in
    return(env, result1 && result2)
  | Or (first, second) -> 
    let return(_, result1) = eval_bool(env, first) return in
    let return(_, result2) = eval_bool(env, second) return in
    return(env, result1 || result2)
  | Bool_op (first, op, second) -> 
    let return(_, result1) = eval_bool(env, first) return in
    let return(_, result2) = eval_bool(env, second) return in
      (match op with
      | Lt -> return(env, result1 < result2)
      | Gt -> return(env, result1 > result2)
      | Eq -> return(env, result1 = result2))
;;

let rec eval_statement (env, statement) return =
  let return1(env, x) = x in 
    (match statement with
    | Skip ->  return(env)
    | Assign (var, exp) ->
      let return1 (_, result) = eval_arith (env, exp)
    | Seq (first, second) -> (failwith "hole")
    | If (cond, true_, false_) -> (failwith "hole")
    | While (cond, body) as loop -> (failwith "hole"))
;;

let eval statement =
  eval_statement
    ((fun x -> failwith @@ "Not found: " ^ x), statement)
    (fun x -> x)
;;
`
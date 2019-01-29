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
  | Var var -> return (env, (failwith "hole"))
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
  | True -> (failwith "hole")
  | False -> (failwith "hole")
  | Not exp -> (failwith "hole")
  | And (first, second) -> (failwith "hole")
  | Or (first, second) -> (failwith "hole")
  | Bool_op (first, op, second) -> (failwith "hole")
;;

let rec eval_statement (env, statement) return =
  match statement with
  | Skip ->  (failwith "hole")
  | Assign (var, exp) -> (failwith "hole")
  | Seq (first, second) -> (failwith "hole")
  | If (cond, true_, false_) -> (failwith "hole")
  | While (cond, body) as loop -> (failwith "hole")
;;

let eval statement =
  eval_statement
    ((fun x -> failwith @@ "Not found: " ^ x), statement)
    (fun x -> x)
;;

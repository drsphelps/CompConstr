[@@@ocaml.warning "-27"]

open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith env = function
  | Var var -> (failwith "hole")
  | Int int -> (failwith "hole")
  | Plus (a, b) -> (failwith "hole")
  | Times (a, b) -> (failwith "hole")
;;

let rec eval_bool env = function
  | True -> (failwith "hole")
  | False -> (failwith "hole")
  | Not exp -> (failwith "hole")
  | And (first, second) -> (failwith "hole")
  | Or (first, second) -> (failwith "hole")
  | Bool_op (first, op, second) -> (failwith "hole")
;;

let rec eval_statement env = function
  | Skip -> (failwith "hole")
  | Assign (var, exp) ->
    let result = (failwith "hole") in
    fun var' -> if (failwith "hole") then (failwith "hole") else (failwith "hole")
  | Seq (first, second) -> (failwith "hole")
  | If (cond, true_, false_) -> (failwith "hole")
  | While (cond, body) as loop -> (failwith "hole")
;;

let eval_statement =
  eval_statement (fun x -> failwith @@ "Not found: " ^ x)
;;

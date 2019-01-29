[@@@ocaml.warning "-27"]

open Ast
;;

type instr =
  | Lookup of string
  | Push_int of int
  | Plus
  | Times
  | Push_bool of bool
  | Not
  | And
  | Or
  | Less_than
  | Greater_than
  | Equals
  | Assign of string
  | If of instr list * instr list
  | While of instr list * instr list
;;

let rec compile_arith = function
  | Var var -> [Lookup var]
  | Int int -> [Push_int int]
  | Plus (a, b) -> compile_arith a @ compile_arith b @ [Plus]
  | Times (a, b) -> compile_arith a @ compile_arith b @ [Times]
;;

let rec compile_bool = function
  | True -> (failwith "hole")
  | False -> (failwith "hole")
  | Not exp -> (failwith "hole")
  | And (first, second) -> (failwith "hole")
  | Or (first, second) -> (failwith "hole")
  | Bool_op (first, op, second) -> (failwith "hole")

let rec compile_statement = function
  | Skip ->  (failwith "hole")
  | Assign (var, exp) -> (failwith "hole")
  | Seq (first, second) -> (failwith "hole")
  | If (cond, true_, false_) -> (failwith "hole")
  | While (cond, body) -> (failwith "hole")
;;

type value =
  | Int of int
  | Bool of bool
;;

type env =
  (string * int) list
;;

let lookup env var =
  Int (List.Assoc.find_exn env ~equal:String.(=) var)
;;

exception Malformed_stack
;;

type stack =
  value list
;;

let one_bool = function
  | Bool a :: stack -> (a, stack)
  | _ ->  raise Malformed_stack
;;

let two_bools = function
  | Bool a :: Bool b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let one_int = function
  | Int a :: stack -> (a, stack)
  | _ -> raise Malformed_stack
;;

let two_ints = function
  | Int a :: Int b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let rec interp (env, stack) = function

  | [] -> (failwith "hole")

  | Lookup var :: rest -> (failwith "hole")

  | Push_int int :: rest -> (failwith "hole")

  | Plus :: rest -> (failwith "hole")

  | Times :: rest -> (failwith "hole")

  | Push_bool bool :: rest -> (failwith "hole")

  | Not :: rest -> (failwith "hole")

  | And :: rest -> (failwith "hole")

  | Or :: rest -> (failwith "hole")

  | Less_than :: rest -> (failwith "hole")

  | Greater_than :: rest -> (failwith "hole")

  | Equals :: rest -> (failwith "hole")

  | Assign var :: rest -> (failwith "hole")

  | If (true_, false_) :: rest -> (failwith "hole")

  | While (cond, body) as loop :: rest -> (failwith "hole")

;;

let eval statement =
  interp ([], []) (compile_statement statement)
;;

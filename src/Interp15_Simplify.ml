[@@@ocaml.warning "-27"]

(* 5: Simplify grouping *)
open Ast
;;

type value =
  | Int of int
  | Bool of bool
;;

type env =
  (string * int) list
;;

let dummy =
  Int 0
;;

let lookup env var =
  Int (List.Assoc.find_exn env ~equal:String.(=) var)
;;

(* Elements of a list must be of the same type a.k.a. *)
(* the coproduct over the three continuation monoids *)
type cont =
  (* arith_cont *)
  | Lookup of string
  | Plus_with of int
  | Times_with of int
  | Compile_then_plus of arith_exp
  | Compile_then_times of arith_exp
  (* returns to eval_bool *)
  | Bool_op_with of bool_op * int
  | Compile_then_bool_op of bool_op * arith_exp
  (* returns to eval_statement *)
  | Update_env_then of string

  (* bool_cont *)
  | Not
  | And_with of bool
  | Or_with of bool
  | Compile_then_and of bool_exp
  | Compile_then_or of bool_exp
  (* returns to eval_statement *)
  | Branch of statement * statement
  | Loop of statement * statement

  (* statement_cont *)
  | Compile_then of statement
;;

let get_int = function
  | Int int -> int
  | Bool _ -> assert false
;;

let get_bool = function
  | Bool bool -> bool
  | Int _ -> assert false
;;

let rec interp (env, value) = function

  | [] -> (failwith "hole")
  | Compile_then second :: rest -> (failwith "hole")

  (* get_int value *)
  | Lookup var :: rest -> interp (env, lookup env var) rest
  | Plus_with int2 :: rest -> interp (env, Int (get_int value + int2)) rest
  | Times_with int2 :: rest -> interp (env, Int (get_int value * int2)) rest
  | Bool_op_with (op, int2) :: rest ->
    let bool = match op with
      | Lt -> get_int value < int2
      | Gt -> get_int value > int2
      | Eq -> get_int value = int2 in
    interp (env, Bool bool) rest
  | Compile_then_plus arith :: rest -> compile_arith (env, arith) (Plus_with (get_int value) :: rest)
  | Compile_then_times arith :: rest -> compile_arith (env, arith) (Times_with (get_int value) :: rest)
  | Compile_then_bool_op (op, arith) :: rest -> compile_arith (env, arith) (Bool_op_with (op, (get_int value)) :: rest)
  | Update_env_then var :: rest -> interp ((var, get_int value) :: env, dummy) rest

  (* get_bool value *)
  | Not :: rest -> (failwith "hole")
  | And_with bool2 :: rest -> (failwith "hole")
  | Or_with bool2 :: rest -> (failwith "hole")
  | Compile_then_and bool_exp :: rest -> (failwith "hole")
  | Compile_then_or bool_exp :: rest -> (failwith "hole")
  | Branch (true_, false_) :: rest -> (failwith "hole")
  | Loop (body, loop) :: rest -> (failwith "hole")

and compile_arith (env, arith) rest =
  match arith with
  | Var var -> interp (env, dummy) (Lookup var :: rest)
  | Int int -> interp (env, Int int) rest
  | Plus (a, b) -> compile_arith (env, a) (Compile_then_plus b :: rest)
  | Times (a, b) -> compile_arith (env, a) (Compile_then_times b :: rest)

and compile_bool (env, bool_exp) rest =
  match bool_exp with
  | True -> (failwith "hole")
  | False -> (failwith "hole")
  | Not exp -> (failwith "hole")
  | And (first, second) -> (failwith "hole")
  | Or (first, second) -> (failwith "hole")
  | Bool_op (first, op, second) -> (failwith "hole")

and compile_statement (env, statement) rest =
  match statement with
  | Skip -> (failwith "hole")
  | Assign (var, arith_exp) -> (failwith "hole")
  | Seq (first, second) -> (failwith "hole")
  | If (cond, true_, false_) -> (failwith "hole")
  | While (cond, body) as loop -> (failwith "hole")
;;

let eval statement =
  compile_statement ([], statement) []
;;


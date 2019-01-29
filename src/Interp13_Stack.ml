[@@@ocaml.warning "-27"]

(* 3: Stackify/instruction list *)
open Ast
;;

type value =
  | None
  | Int of int
  | Bool of bool
;;

type env =
  (string * int) list
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

let rec interp_arith (env, int) =
  match int with None | Bool _ -> assert false | Int int ->
  function
  | [] -> (env, Int int)
  | Lookup var :: rest -> interp_arith (env, lookup env var) rest
  | Plus_with int2 :: rest -> interp_arith (env, Int (int + int2)) rest
  | Times_with int2 :: rest -> interp_arith (env, Int (int * int2)) rest
  | Bool_op_with (op, int2) :: rest ->
    let bool = match op with
      | Lt -> int < int2
      | Gt -> int > int2
      | Eq -> int = int2 in
    interp_bool (env, Bool bool) rest
  | Compile_then_plus arith :: rest -> compile_arith (env, arith) (Plus_with int :: rest)
  | Compile_then_times arith :: rest -> compile_arith (env, arith) (Times_with int :: rest)
  | Compile_then_bool_op (op, arith) :: rest -> compile_arith (env, arith) (Bool_op_with (op, int) :: rest)
  | Update_env_then var :: rest -> interp_statement ((var, int) :: env) rest
  | _ -> assert false

and compile_arith (env, arith) rest =
  match arith with
  | Var var -> interp_arith (env, None) (Lookup var :: rest)
  | Int int -> interp_arith (env, Int int) rest
  | Plus (a, b) -> compile_arith (env, a) (Compile_then_plus b :: rest)
  | Times (a, b) -> compile_arith (env, a) (Compile_then_times b :: rest)

and interp_bool (env, bool) =
  match bool with None | Int _ -> assert false | Bool bool ->
  function
  | [] -> (failwith "hole")
  | Not :: rest -> (failwith "hole")
  | And_with bool2 :: rest -> (failwith "hole")
  | Or_with bool2 :: rest -> (failwith "hole")
  | Compile_then_and bool_exp :: rest -> (failwith "hole")
  | Compile_then_or bool_exp :: rest -> (failwith "hole")
  | Branch (true_, false_) :: rest -> (failwith "hole")
  | Loop (body, loop) :: rest -> (failwith "hole")
  | _ -> assert false

and compile_bool (env, bool_exp) rest =
  match bool_exp with
  | True -> (failwith "hole")
  | False -> (failwith "hole")
  | Not exp -> (failwith "hole")
  | And (first, second) -> (failwith "hole")
  | Or (first, second) -> (failwith "hole")
  | Bool_op (first, op, second) -> (failwith "hole")

and interp_statement env = function
  | [] -> (failwith "hole")
  | Compile_then second :: rest -> (failwith "hole")
  | _ -> assert false

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

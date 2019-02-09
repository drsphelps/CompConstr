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
  | True -> [Push_bool true]
  | False -> [Push_bool false]
  | Not exp -> compile_bool exp @ [Not]
  | And (first, second) -> compile_bool first @ compile_bool second @ [And]
  | Or (first, second) -> compile_bool first @ compile_bool second @ [Or]
  | Bool_op (first, op, second) -> let result = compile_arith first @ compile_arith second in
                                   match op with
                                   | Lt -> result @ [Less_than]
                                   | Gt -> result @ [Greater_than]
                                   | Eq -> result @ [Equals]

let rec compile_statement = function
  | Skip ->  []
  | Assign (var, exp) -> [Assign var] @ (compile_arith exp)
  | Seq (first, second) -> compile_statement first @ compile_statement second
  | If (cond, true_, false_) -> (compile_bool cond) @  [If (compile_statement true_, compile_statement false_)]
  | While (cond, body) -> let res = compile_bool cond in 
    res @ [While (res, compile_statement body)]
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

  | [] -> (env, stack)

  | Lookup var :: rest -> interp (env, (lookup env var) :: stack) rest

  | Push_int int :: rest -> interp (env, Int int :: stack) rest 

  | Plus :: rest -> 
    let (a, b, stack') = two_ints stack in
    interp (env, Int (a + b) :: stack') rest

  | Times :: rest -> 
    let (a, b, stack') = two_ints stack in
    interp (env, Int (a * b) :: stack') rest

  | Push_bool bool :: rest -> interp (env, Bool bool :: stack) rest 

  | Not :: rest -> 
    let (a, stack') = one_bool stack in
    interp (env, Bool (not a) :: stack') rest

  | And :: rest -> 
    let (a, b, stack') = two_bools stack in
    if a && b then interp (env, Bool true :: stack') rest
    else interp (env, Bool false :: stack') rest

  | Or :: rest -> 
    let (a, b, stack') = two_bools stack in
    if a || b then interp (env, Bool true :: stack') rest
    else interp (env, Bool false :: stack') rest

  | Less_than :: rest -> 
    let (a, b, stack') = two_ints stack in
    if a < b then interp (env, Bool true :: stack') rest
    else interp (env, Bool false :: stack') rest

  | Greater_than :: rest -> 
    let (a, b, stack') = two_ints stack in
    if a > b then interp (env, Bool true :: stack') rest
    else interp (env, Bool false :: stack') rest

  | Equals :: rest -> 
    let (a, b, stack') = two_ints stack in 
    if a = b then interp (env, Bool true :: stack') rest
    else interp (env, Bool false :: stack') rest

  | Assign var :: rest -> 
    let (a, stack') = one_int stack in
    interp ((var, a) :: env, stack') rest

  | If (true_, false_) :: rest -> 
    let (a, stack') = one_bool stack in
    (match a with
    | true -> interp (env, stack') (true_ @ rest)
    | false -> interp (env, stack') (false_ @ rest))

  | While (cond, body) as loop :: rest -> 
    let (b, stack') = one_bool stack in
      (match b with
      | true -> interp (env, stack') (body @ [loop] @ rest)
      | false -> interp (env, stack') rest)

;;

let eval statement =
  interp ([], []) (compile_statement statement)
;;

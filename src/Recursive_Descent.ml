open Lexer
;;

type arith_exp =
  | Var of string
  | Int of int
  | Plus of arith_exp * arith_exp
  | Times of arith_exp * arith_exp
;;

type bool_op =
  | Lt
  | Gt
  | Eq
;;

type bool_exp =
  | True
  | False
  | Not of bool_exp
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp
  | Bool_op of arith_exp * bool_op * arith_exp
;;

type statement =
  | Skip
  | Assign of string * arith_exp
  | Seq of statement * statement
  | If of bool_exp * statement * statement
  | While of bool_exp * statement
;;

exception Parse_empty
;;

exception Parse_error of token list
;;

let (=) =
  [%compare.equal : token]
;;

let then_expect expected (result, rest) =
  match rest with
  | [] -> raise Parse_empty
  | token :: rest ->
    if expected = token then (result, rest) else raise @@ Parse_error rest
;;

let eat expected = function
  | [] -> raise Parse_empty
  | token :: rest -> if expected = token then rest else raise @@ Parse_error rest
;;

let rec statement = function
  | [] -> (Skip, [])

  | ID var :: rest ->
    let arith, rest = rest |> eat COLON_EQ |> arith_exp in
    statement_more (Assign (var, arith)) rest

  | WHILE :: rest ->
    let bool, rest = bool_exp rest in
    let do_, rest = rest |> eat DO |> statement in
    statement_more (While (bool, do_)) rest

  | IF :: rest ->
    let bool, rest = bool_exp rest in
    let true_, rest = rest |> eat THEN |> statement in
    let false_, rest = rest |> eat ELSE |> statement in
    statement_more (If (bool, true_, false_)) rest

  | LEFT_CURLY :: rest ->
    statement rest |> then_expect RIGHT_CURLY

  | rest -> raise @@ Parse_error rest

and statement_more seen = function

  | SEMICOLON :: rest ->
    let statement, rest = statement rest in
    statement_more (Seq (seen, statement)) rest

  | rest -> (seen, rest)

and bool_exp = function

  | [] -> raise Parse_empty

  | TRUE :: rest ->
    bool_more True rest

  | FALSE :: rest ->
    bool_more False rest

  | BANG :: rest ->
    let bool, rest = bool_exp rest in
    bool_more (Not bool) rest

  | LEFT_PAREN :: rest ->
    let bool, rest = bool_exp rest |> then_expect RIGHT_PAREN in
    bool_more bool rest

  | ID var :: rest ->
    let first, rest = arith_more (Var var) rest in
    let op, rest =
      begin match rest with
        | [] -> raise Parse_empty
        | LESS_THAN :: rest -> (Lt, rest)
        | GREATER_THAN :: rest -> (Gt, rest)
        | EQUALS :: rest -> (Eq, rest)
        | _ :: _ as rest -> raise @@ Parse_error rest
      end in
    let second, rest = arith_exp rest in
    bool_more (Bool_op (first, op, second)) rest

  | NUM n :: rest ->
    let first, rest = arith_more (Int n) rest in
    let op, rest =
      begin match rest with
        | [] -> raise Parse_empty
        | LESS_THAN :: rest -> (Lt, rest)
        | GREATER_THAN :: rest -> (Gt, rest)
        | EQUALS :: rest -> (Eq, rest)
        | _ :: _ as rest -> raise @@ Parse_error rest
      end in
    let second, rest = arith_exp rest in
    bool_more (Bool_op (first, op, second)) rest

  | rest -> raise @@ Parse_error rest

and bool_more seen = function

  | AND :: rest ->
    let bool, rest = bool_exp rest in
    bool_more (And (seen, bool)) rest

  | OR :: rest ->
    let bool, rest = bool_exp rest in
    bool_more (Or (seen, bool)) rest

  | rest -> (seen, rest)

and arith_exp = function
  | [] -> raise Parse_empty

  | NUM n :: rest ->
    arith_more (Int n) rest

  | ID var :: rest ->
    arith_more (Var var) rest

  | LEFT_PAREN :: rest ->
    let arith, rest = arith_exp rest |> then_expect RIGHT_PAREN in
    arith_more arith rest

  | rest -> raise @@ Parse_error rest

and arith_more seen = function

  | PLUS :: rest ->
    let arith, rest = arith_exp rest in
    arith_more (Plus (seen, arith)) rest

  | STAR ::  rest ->
    let arith, rest = arith_exp rest in
    arith_more (Times (seen, arith)) rest

  | rest -> (seen, rest)
;;

let parse tokens =
  try
    Result.return @@ statement tokens
  with Parse_error rest -> Result.fail rest
     | Parse_empty -> Result.fail []
;;

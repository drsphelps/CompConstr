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

type result =
  | Arith of arith_exp
  | Bool of bool_exp
  | Stmt of statement
  | Bool_op of bool_op
;;

type parse_instruction = 
  | Eat of token

  | If_semicolon_then_stmt
  | Push_statement

  | Push_bool
  | Push_bool_op
  | If_and_or_or_then_bool

  | Push_arith
  | If_plus_or_star_then_arith

  | Make_assign
  | Make_while
  | Make_if
  | Make_seq

  | Make_not
  | Make_bool_op
  | Make_and
  | Make_or

  | Make_plus
  | Make_times
;;

exception Parse_empty
;;

exception Parse_error of parse_instruction list * result list * token list
;;

let (=) =
  [%compare.equal : token]
;;

let rec loop = function

  | [], [Stmt ans], [] ->
    ans

  (* Not modifying stack *)

  | Eat tok :: instr, stack, tok' :: rest ->
    if tok = tok' then loop (instr, stack, rest) else raise @@ Parse_error (Eat tok :: instr, stack, tok' :: rest)

  (* Statement *)

  | Push_statement :: instr, stack, ID var :: rest ->
    loop (Eat COLON_EQ :: Push_arith :: Make_assign :: If_semicolon_then_stmt :: instr, Arith (Var var) :: stack, rest)

  | Push_statement :: instr, stack, WHILE :: rest ->
    loop (Push_bool :: Eat DO :: Push_statement :: Make_while :: If_semicolon_then_stmt :: instr, stack, rest)

  | Push_statement :: instr, stack, IF :: rest ->
    loop (Push_bool :: Eat THEN :: Push_statement :: Eat ELSE :: Push_statement :: Make_if :: If_semicolon_then_stmt :: instr, stack, rest)

  | Push_statement :: instr, stack, LEFT_CURLY :: rest ->
    loop (Push_statement :: Eat RIGHT_CURLY :: If_semicolon_then_stmt :: instr, stack, rest)

  | If_semicolon_then_stmt :: instr, stack, SEMICOLON :: rest ->
    loop (Push_statement :: Make_seq :: If_semicolon_then_stmt :: instr, stack, rest)

  | If_semicolon_then_stmt :: instr, stack, rest ->
    loop (instr, stack, rest)

  (* Boolean expressions *)

  | Push_bool :: instr, stack, TRUE :: rest ->
    loop (If_and_or_or_then_bool :: instr, Bool True :: stack, rest)

  | Push_bool :: instr, stack, FALSE :: rest ->
    loop (If_and_or_or_then_bool :: instr, Bool False :: stack, rest)

  | Push_bool :: instr, stack, BANG :: rest ->
    loop (Push_bool :: Make_not :: If_and_or_or_then_bool :: instr, stack, rest)

  | Push_bool :: instr, stack, LEFT_PAREN :: rest ->
    loop (Push_bool :: Eat RIGHT_PAREN :: If_and_or_or_then_bool :: instr, stack, rest)

  | Push_bool :: instr, stack, ID var :: rest ->
    loop (If_plus_or_star_then_arith :: Push_bool_op :: Push_arith :: Make_bool_op :: If_and_or_or_then_bool :: instr, (Arith (Var var)) :: stack, rest)

  | Push_bool :: instr, stack, NUM num :: rest ->
    loop (If_plus_or_star_then_arith :: Push_bool_op :: Push_arith :: Make_bool_op :: If_and_or_or_then_bool :: instr, (Arith (Int num)) :: stack, rest)

  | If_and_or_or_then_bool :: instr, stack, AND :: rest ->
    loop (Push_bool :: Make_and :: If_and_or_or_then_bool :: instr, stack, rest)

  | If_and_or_or_then_bool :: instr, stack, OR :: rest ->
    loop (Push_bool :: Make_or :: If_and_or_or_then_bool :: instr, stack, rest)

  | If_and_or_or_then_bool :: instr, stack, rest ->
    loop (instr, stack, rest)

  | Push_bool_op :: instr, stack, LESS_THAN :: rest ->
    loop (instr, (Bool_op Lt) :: stack, rest)

  | Push_bool_op :: instr, stack, EQUALS :: rest ->
    loop (instr, (Bool_op Eq) :: stack, rest)

  | Push_bool_op :: instr, stack, GREATER_THAN :: rest ->
    loop (instr, (Bool_op Gt) :: stack, rest)

  (* Arithmetic *)

  | Push_arith :: instr, stack, NUM n :: rest ->
    loop (If_plus_or_star_then_arith :: instr, (Arith (Int n)) :: stack, rest)

  | Push_arith :: instr, stack, ID var :: rest ->
    loop (If_plus_or_star_then_arith :: instr, (Arith (Var var)) :: stack, rest)

  | Push_arith :: instr, stack, LEFT_PAREN :: rest ->
    loop (Push_arith :: If_plus_or_star_then_arith :: instr, stack, rest)

  | If_plus_or_star_then_arith :: instr, stack, PLUS :: rest ->
    loop (Push_arith :: Make_plus :: If_plus_or_star_then_arith :: instr, stack, rest)

  | If_plus_or_star_then_arith :: instr, stack, STAR :: rest ->
    loop (Push_arith :: Make_times :: If_plus_or_star_then_arith :: instr, stack, rest)

  | If_plus_or_star_then_arith :: instr, stack, rest ->
    loop (instr, stack, rest)

  (* Modifying stack *)

  (* Statments *)

  | Make_assign :: instr, (Arith exp) :: (Arith (Var var)) :: stack, rest ->
    loop (instr, Stmt (Assign (var, exp)) :: stack, rest)

  | Make_while :: instr, (Stmt body) :: (Bool cond) :: stack, rest ->
    loop (instr, Stmt (While (cond, body)) :: stack, rest)

  | Make_seq :: instr, (Stmt s2) :: (Stmt s1) :: stack, rest ->
    loop (instr, Stmt (Seq (s1, s2)) :: stack, rest)

  | Make_if :: instr, (Stmt false_) :: (Stmt true_) :: (Bool cond) :: stack, rest ->
    loop (instr, Stmt (If (cond, true_, false_)) :: stack, rest)

  (* Boolean expressions *)

  | Make_not :: instr, (Bool exp) :: stack, rest ->
    loop (instr, Bool (Not exp) :: stack, rest)

  | Make_and :: instr, (Bool exp2) :: (Bool exp1) :: stack, rest ->
    loop (instr, Bool (And (exp1, exp2)) :: stack, rest)

  | Make_or :: instr, (Bool exp2) :: (Bool exp1) :: stack, rest ->
    loop (instr, Bool (Or (exp1, exp2)) :: stack, rest)

  | Make_bool_op :: instr, (Arith exp2) :: (Bool_op op) :: (Arith exp1) :: stack, rest -> 
    loop (instr, Bool (Bool_op (exp1, op, exp2)) :: stack, rest)

  (* Arithmetic *)

  | Make_plus :: instr, (Arith exp2) :: (Arith exp1) :: stack, rest ->
    loop (instr, Arith (Plus (exp1, exp2)) :: stack, rest)

  | Make_times :: instr, (Arith exp2) :: (Arith exp1) :: stack, rest ->
    loop (instr, Arith (Times (exp1, exp2)) :: stack, rest)

  (* Exception *)
  | instr, stack, rest ->
    raise @@ Parse_error (instr, stack, rest)
;;

let parse tokens =
  try
    Result.return @@ loop ([Push_statement], [], tokens)
  with Parse_error (instr, stack, rest) -> Result.fail (instr, stack, rest)
;;


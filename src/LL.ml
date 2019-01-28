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

type symbol = 
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

exception Parse_error of symbol list * result list * token list
;;

let (=) =
  [%compare.equal : token]
;;

let rec loop = function

  | [], [Stmt ans], [] ->
    ans

  (* Not modifying scratch *)

  | Eat tok :: stack, scratch, tok' :: stream ->
    if tok = tok' then loop (stack, scratch, stream) else raise @@ Parse_error (Eat tok :: stack, scratch, tok' :: stream)

  (* Statement *)

  | Push_statement :: stack, scratch, ID var :: stream ->
    loop (Eat COLON_EQ :: Push_arith :: Make_assign :: If_semicolon_then_stmt :: stack, Arith (Var var) :: scratch, stream)

  | Push_statement :: stack, scratch, WHILE :: stream ->
    loop (Push_bool :: Eat DO :: Push_statement :: Make_while :: If_semicolon_then_stmt :: stack, scratch, stream)

  | Push_statement :: stack, scratch, IF :: stream ->
    loop (Push_bool :: Eat THEN :: Push_statement :: Eat ELSE :: Push_statement :: Make_if :: If_semicolon_then_stmt :: stack, scratch, stream)

  | Push_statement :: stack, scratch, LEFT_CURLY :: stream ->
    loop (Push_statement :: Eat RIGHT_CURLY :: If_semicolon_then_stmt :: stack, scratch, stream)

  | If_semicolon_then_stmt :: stack, scratch, SEMICOLON :: stream ->
    loop (Push_statement :: Make_seq :: If_semicolon_then_stmt :: stack, scratch, stream)

  | If_semicolon_then_stmt :: stack, scratch, stream ->
    loop (stack, scratch, stream)

  (* Boolean expressions *)

  | Push_bool :: stack, scratch, TRUE :: stream ->
    loop (If_and_or_or_then_bool :: stack, Bool True :: scratch, stream)

  | Push_bool :: stack, scratch, FALSE :: stream ->
    loop (If_and_or_or_then_bool :: stack, Bool False :: scratch, stream)

  | Push_bool :: stack, scratch, BANG :: stream ->
    loop (Push_bool :: Make_not :: If_and_or_or_then_bool :: stack, scratch, stream)

  | Push_bool :: stack, scratch, LEFT_PAREN :: stream ->
    loop (Push_bool :: Eat RIGHT_PAREN :: If_and_or_or_then_bool :: stack, scratch, stream)

  | Push_bool :: stack, scratch, ID var :: stream ->
    loop (If_plus_or_star_then_arith :: Push_bool_op :: Push_arith :: Make_bool_op :: If_and_or_or_then_bool :: stack, (Arith (Var var)) :: scratch, stream)

  | Push_bool :: stack, scratch, NUM num :: stream ->
    loop (If_plus_or_star_then_arith :: Push_bool_op :: Push_arith :: Make_bool_op :: If_and_or_or_then_bool :: stack, (Arith (Int num)) :: scratch, stream)

  | If_and_or_or_then_bool :: stack, scratch, AND :: stream ->
    loop (Push_bool :: Make_and :: If_and_or_or_then_bool :: stack, scratch, stream)

  | If_and_or_or_then_bool :: stack, scratch, OR :: stream ->
    loop (Push_bool :: Make_or :: If_and_or_or_then_bool :: stack, scratch, stream)

  | If_and_or_or_then_bool :: stack, scratch, stream ->
    loop (stack, scratch, stream)

  | Push_bool_op :: stack, scratch, LESS_THAN :: stream ->
    loop (stack, (Bool_op Lt) :: scratch, stream)

  | Push_bool_op :: stack, scratch, EQUALS :: stream ->
    loop (stack, (Bool_op Eq) :: scratch, stream)

  | Push_bool_op :: stack, scratch, GREATER_THAN :: stream ->
    loop (stack, (Bool_op Gt) :: scratch, stream)

  (* Arithmetic *)

  | Push_arith :: stack, scratch, NUM n :: stream ->
    loop (If_plus_or_star_then_arith :: stack, (Arith (Int n)) :: scratch, stream)

  | Push_arith :: stack, scratch, ID var :: stream ->
    loop (If_plus_or_star_then_arith :: stack, (Arith (Var var)) :: scratch, stream)

  | Push_arith :: stack, scratch, LEFT_PAREN :: stream ->
    loop (Push_arith :: If_plus_or_star_then_arith :: stack, scratch, stream)

  | If_plus_or_star_then_arith :: stack, scratch, PLUS :: stream ->
    loop (Push_arith :: Make_plus :: If_plus_or_star_then_arith :: stack, scratch, stream)

  | If_plus_or_star_then_arith :: stack, scratch, STAR :: stream ->
    loop (Push_arith :: Make_times :: If_plus_or_star_then_arith :: stack, scratch, stream)

  | If_plus_or_star_then_arith :: stack, scratch, stream ->
    loop (stack, scratch, stream)

  (* Modifying scratch *)

  (* Statments *)

  | Make_assign :: stack, (Arith exp) :: (Arith (Var var)) :: scratch, stream ->
    loop (stack, Stmt (Assign (var, exp)) :: scratch, stream)

  | Make_while :: stack, (Stmt body) :: (Bool cond) :: scratch, stream ->
    loop (stack, Stmt (While (cond, body)) :: scratch, stream)

  | Make_seq :: stack, (Stmt s2) :: (Stmt s1) :: scratch, stream ->
    loop (stack, Stmt (Seq (s1, s2)) :: scratch, stream)

  | Make_if :: stack, (Stmt false_) :: (Stmt true_) :: (Bool cond) :: scratch, stream ->
    loop (stack, Stmt (If (cond, true_, false_)) :: scratch, stream)

  (* Boolean expressions *)

  | Make_not :: stack, (Bool exp) :: scratch, stream ->
    loop (stack, Bool (Not exp) :: scratch, stream)

  | Make_and :: stack, (Bool exp2) :: (Bool exp1) :: scratch, stream ->
    loop (stack, Bool (And (exp1, exp2)) :: scratch, stream)

  | Make_or :: stack, (Bool exp2) :: (Bool exp1) :: scratch, stream ->
    loop (stack, Bool (Or (exp1, exp2)) :: scratch, stream)

  | Make_bool_op :: stack, (Arith exp2) :: (Bool_op op) :: (Arith exp1) :: scratch, stream -> 
    loop (stack, Bool (Bool_op (exp1, op, exp2)) :: scratch, stream)

  (* Arithmetic *)

  | Make_plus :: stack, (Arith exp2) :: (Arith exp1) :: scratch, stream ->
    loop (stack, Arith (Plus (exp1, exp2)) :: scratch, stream)

  | Make_times :: stack, (Arith exp2) :: (Arith exp1) :: scratch, stream ->
    loop (stack, Arith (Times (exp1, exp2)) :: scratch, stream)

  (* Exception *)
  | stack, scratch, stream ->
    raise @@ Parse_error (stack, scratch, stream)
;;

let parse token_stream =
  try
    Result.return @@ loop ([Push_statement], [], token_stream)
  with Parse_error (stack, scratch, stream) -> Result.fail (stack, scratch, stream)
;;


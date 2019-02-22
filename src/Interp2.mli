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
  | Pop_env
  | Pop_stack
  | Push_closure of compiled_closure
  | Call

  and compiled_closure = {
  arg : string;
  body : instr list;
}  

val compile_arith : Ast0.arith_exp -> instr list

val compile_bool: Ast0.bool_exp -> instr list

val compile_statement: Ast0.statement -> instr list

type value =
  | Int of int
  | Bool of bool
  | Closure of compiled_closure

type env =
  (string * (int, compiled_closure) Either.t) list

val lookup: (string, (int, compiled_closure) Either.t) List.Assoc.t -> string -> value

type stack =
  value list

val one_bool: value list -> bool * value list
val two_bools: value list -> bool * bool * value list
val one_int: value list -> int * value list
val two_ints: value list -> int * int * value list

val interp: (string, (int, compiled_closure) Either.t) List.Assoc.t * value list ->
instr list ->
(string, (int, compiled_closure) Either.t) List.Assoc.t * value list

val eval: Ast0.statement -> (string, (int, compiled_closure) Either.t) List.Assoc.t * value list
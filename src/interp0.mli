type value = int

type env = string -> (value, Ast0.closure) Either.t option

val get_int : (string -> ('a, 'b) Either.t option) -> string -> 'a

val get_func : (string -> ('a, 'b) Either.t option) -> string -> 'b

val eval_arith : env -> Ast0.arith_exp -> value

val eval_bool : env -> Ast0.bool_exp -> bool

val eval_statement : env -> Ast0.statement -> env

val eval_statement : Ast0.statement -> env
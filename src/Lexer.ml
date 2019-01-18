type token =
  | AND
  | BANG
  | COLON_EQ
  | DO
  | ELSE
  | EQUALS
  | FALSE
  | GREATER_THAN
  | ID of string
  | IF
  | LEFT_CURLY
  | LEFT_PAREN
  | LESS_THAN
  | NUM of int
  | OR
  | PLUS
  | RIGHT_CURLY
  | RIGHT_PAREN
  | SEMICOLON
  | SKIP
  | STAR
  | THEN
  | TRUE
  | WHILE
[@@deriving sexp_of, compare]
;;

exception LexError of string
;;

let rec no_info ans = function
  | [] -> ans
  | top :: rest ->
    if Char.is_whitespace top then
      no_info ans rest
    else if Char.is_digit top then 
      start_num ans [top] rest
    else if Char.is_alpha top then
      keyword_or_id ans top rest
    else 
      symbol ans rest top

and not_alphanum ans top rest =
  let () = assert (not @@ Char.is_alphanum top) in
  if Char.is_whitespace top then
    no_info ans rest
  else
    symbol ans rest top

and not_symbol ans prev = function
  | [] -> ans
  | top :: rest ->
    if Char.is_whitespace top then
      no_info ans rest
    else if Char.is_alpha top then
      keyword_or_id ans top rest
    else if Char.is_digit top then 
      start_num ans [top] rest
    else 
      raise @@ LexError ("unrecognised token, starts with\n" ^ String.of_char_list [prev; top])

and start_num ans saved = function
  | [] -> NUM (Int.of_string @@ String.of_char_list @@ List.rev saved) :: ans
  | top :: rest ->
    if Char.is_digit top then
      start_num ans (top :: saved) rest
    else if Char.is_alpha top then
      raise @@ LexError ("probably unintended, starts with\n" ^ (String.of_char_list (List.rev (top :: saved))))
    else
      not_alphanum (NUM (Int.of_string @@ String.of_char_list @@ List.rev saved) :: ans) top rest

and symbol ans rest top =
  match top with
  | '!' -> not_symbol (BANG :: ans) top rest
  | '(' -> not_symbol (LEFT_PAREN :: ans) top rest
  | ')' -> not_symbol (RIGHT_PAREN :: ans) top rest
  | '*' -> not_symbol (STAR :: ans) top rest
  | '+' -> not_symbol (PLUS :: ans) top rest
  | ':' ->
    begin match rest with
      | '=' :: rest -> not_symbol (COLON_EQ :: ans) top rest
      | _ -> raise @@ LexError "unrecognised token\n:"
    end
  | ';' -> not_symbol (SEMICOLON :: ans) top rest
  | '<' -> not_symbol (LESS_THAN :: ans) top rest
  | '=' -> not_symbol (EQUALS :: ans) top rest
  | '>' -> not_symbol (GREATER_THAN :: ans) top rest
  | '{' -> not_symbol (LEFT_CURLY :: ans) top rest
  | '}' -> not_symbol (RIGHT_CURLY :: ans) top rest
  | _ -> raise @@ LexError ("unrecognised symbol\n" ^ String.of_char top)

(* Keywords: AND, DO, ELSE, FALSE, IF, OR, THEN, TRUE, WHILE *)
(* Or ID / identifier *)
and keyword_or_id ans top rest =
  let () = assert (Char.is_alpha top) in
  let saved = [top] in
  begin match top with
    | 'a' -> lex_keyword ans AND saved ~keyword:['n'; 'd'] rest
    | 'd' -> lex_keyword ans DO saved ~keyword:['o'] rest
    | 'e' -> lex_keyword ans ELSE saved ~keyword:['l'; 's'; 'e'] rest
    | 'f' -> lex_keyword ans FALSE saved ~keyword:['a'; 'l'; 's'; 'e'] rest
    | 'i' -> lex_keyword ans IF saved ~keyword:['f'] rest
    | 'o' -> lex_keyword ans OR saved ~keyword:['r'] rest
    | 't' ->
      begin match rest with
        | [] -> ID "t" :: ans
        | 'h' :: rest -> lex_keyword ans THEN ('h' :: saved) ~keyword:['e'; 'n'] rest
        | 'r' :: rest -> lex_keyword ans TRUE ('r' :: saved) ~keyword:['u'; 'e'] rest
        | top :: rest ->
          if Char.is_alphanum top then
            id (top :: saved) ans rest
          else
            not_alphanum (ID "t" :: ans) top rest
      end
    | 'w' -> lex_keyword ans WHILE saved ~keyword:['h'; 'i'; 'l'; 'e'] rest
    | _ -> id [top] ans rest
  end

and lex_keyword ans token saved ~keyword rest =
  begin match keyword, rest with

    | (k :: keyword), (top :: rest) ->
      if Char.(k = top) then
        lex_keyword ans token (top :: saved) ~keyword rest
      else if Char.is_alphanum top then
        id (top :: saved) ans rest
      else 
        not_alphanum (ID (String.of_char_list (List.rev saved)) :: ans) top rest

    | [], top :: rest ->
      if Char.is_alphanum top then
        id (top :: saved) ans rest
      else
        not_alphanum (token :: ans) top rest

    | _, [] ->
      ID (String.of_char_list (List.rev saved)) :: ans
  end

and id saved ans = function
  | [] -> ans
  | top :: rest ->
    if Char.is_alphanum top then
      id (top :: saved) ans rest
    else
      not_alphanum (ID (String.of_char_list @@ List.rev saved) :: ans) top rest
;;

let lex string = 
  try 
    string 
    |> String.to_list
    |> no_info []
    |> List.rev
    |> Result.return
  with LexError msg ->
    Result.fail msg
;;

type state = int

type nfa =
  { number_of_states: int
  ; start_state: int
  ; transitions: (int * char * int) list
  ; epsilon_transitions: (int * int) list
  ; end_state: int }

type regex =
  | Null
  | Char of char
  | Star of regex
  | Concat of regex * regex
  | Union of regex * regex

let disjoint base_num
    {number_of_states; start_state; transitions; epsilon_transitions; end_state}
    =
  { number_of_states
  ; start_state= base_num + start_state
  ; transitions=
      List.map transitions ~f:(fun (to_, via, from) ->
          (base_num + to_, via, base_num + from) )
  ; epsilon_transitions
  ; end_state }

let rec to_nfa = function
  | Null ->
      { number_of_states= 1
      ; start_state= 1
      ; transitions= []
      ; epsilon_transitions= []
      ; end_state= 1 }
  | Char c ->
      { number_of_states= 2
      ; start_state= 1
      ; transitions= [(1, c, 0)]
      ; epsilon_transitions= []
      ; end_state= 0 }
  | Star regex ->
      let { number_of_states= old_num
          ; start_state= old_start
          ; transitions
          ; epsilon_transitions= eps
          ; end_state= old_end } =
        to_nfa regex
      in
      let new_num = old_num + 1 in
      let new_start, new_end = (new_num, new_num) in
      { number_of_states= new_num
      ; start_state= new_start
      ; transitions
      ; epsilon_transitions=
          (new_start, old_start) :: (new_end, old_end) :: eps
      ; end_state= new_end }
  | Concat (first, second) ->
      let { number_of_states= second_num
          ; start_state= second_start
          ; transitions= second_trans
          ; epsilon_transitions= second_eps
          ; end_state= second_end } =
        to_nfa second
      in
      let { number_of_states= first_num
          ; start_state= first_start
          ; transitions= first_trans
          ; epsilon_transitions= first_eps
          ; end_state= first_end } =
        disjoint second_num @@ to_nfa first
      in
      { number_of_states= first_num + second_num
      ; start_state= first_start
      ; transitions= first_trans @ second_trans
      ; epsilon_transitions=
          ((first_end, second_start) :: first_eps) @ second_eps
      ; end_state= second_end }
  | Union (first, second) ->
      let { number_of_states= second_num
          ; start_state= second_start
          ; transitions= second_trans
          ; epsilon_transitions= second_eps
          ; end_state= second_end } =
        to_nfa second
      in
      let { number_of_states= first_num
          ; start_state= first_start
          ; transitions= first_trans
          ; epsilon_transitions= first_eps
          ; end_state= first_end } =
        disjoint second_num @@ to_nfa first
      in
      let new_num = first_num + second_num + 2 in
      let new_start = new_num and new_end = new_num - 1 in
      { number_of_states= new_num
      ; start_state= new_start
      ; transitions= first_trans @ second_trans
      ; epsilon_transitions=
          (new_start, first_start) :: (new_start, second_start)
          :: (first_end, new_end) :: (second_end, new_end) :: first_eps
          @ second_eps
      ; end_state= new_end }

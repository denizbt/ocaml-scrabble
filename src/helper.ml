(*Contains all the helper methods that are used across multiple files*)

(* Given a list and an element, returns that list without the first appreance of
   that element. Helper function for play_tiles. *)
let rec list_without_elem (lst : 'a list) (elem : 'a) : 'a list =
  match lst with
  | [] -> lst
  | h :: t -> if h = elem then t else h :: list_without_elem t elem

(*creates a dictionary*)
let rec create_dictionary : string list =
  let file = "src/scrabble_dictionary.txt" in
  let dict = file |> In_channel.open_bin |> In_channel.input_all in
  String.split_on_char '\n' dict

(* Given a string [input], returns a char list representation of that string.
   Multipurpose helper function for *)
let rec char_list_of_string (input : string) : char list =
  match input with
  | "" -> []
  | str ->
      str.[0] :: char_list_of_string (String.sub str 1 (String.length str - 1))

let rec string_of_char_list (input : char list) : string =
  match input with
  | [] -> ""
  | h :: t -> String.make 1 h ^ string_of_char_list t

(* Given an inputted string and a string list dictionary, checks the dictionary
   if the input is in there*)
let rec search_dict (input : string) (dict_lst : string list) : bool =
  match dict_lst with
  | [] -> false
  | h :: t ->
      if String.get h 0 > String.get input 0 then false
      else if h = input then true
      else search_dict input t

(* Creates dictionary as a string list and returns whether the word is in the
   dictionary. Helper function used in check_word*)
let in_dictionary (input : string) : bool =
  search_dict (String.uppercase_ascii input ^ "\r") create_dictionary

(* Makes sure that the given location has either the same letter, or the same
   integer constant (either completely horizontal or vertical direction). *)
let rec valid_dir (loc : (char * int) * (char * int)) : bool =
  let fst_char = fst (fst loc) in
  let snd_char = fst (snd loc) in
  let fst_int = snd (fst loc) in
  let snd_int = snd (snd loc) in
  if fst_char = snd_char && fst_int <= snd_int then true
  else if fst_int = snd_int && int_of_char fst_char <= int_of_char snd_char then
    true
  else false

let valid_loc_length loc word : bool =
  let fst_char = fst (fst loc) in
  let snd_char = fst (snd loc) in
  let fst_int = snd (fst loc) in
  let snd_int = snd (snd loc) in
  if fst_char = snd_char then
    if snd_int - fst_int + 1 <> String.length word then false else true
  else if fst_int = snd_int then
    if int_of_char snd_char - int_of_char fst_char + 1 <> String.length word
    then false
    else true
  else false

(* Prevents program from crashing by ensuring that you do not record an out of
   bounds location. *)
let loc_in_bounds (loc : (char * int) * (char * int)) : bool =
  let fst_char = int_of_char (fst (fst loc)) in
  let snd_char = int_of_char (fst (snd loc)) in
  let fst_int = snd (fst loc) in
  let snd_int = snd (snd loc) in
  if
    fst_char < 65 || fst_char > 79 || snd_char < 65 || snd_char > 79
    || fst_int > 14 || snd_int > 14 || fst_int < 0 || snd_int < 0
    || fst_int > snd_int
  then false
  else true

(* Returns location tuple where int is double digits. *)
let helper_double_digit (loc : string) : char * int =
  (loc.[0], int_of_string (String.make 1 loc.[1] ^ String.make 1 loc.[2]))

let gen_loc (loc : string) =
  if loc = "" then (('a', -1), ('a', -1))
  else
    match
      loc |> String.split_on_char '-' |> List.filter (fun s -> s <> " ")
    with
    | [ start; end_ ] ->
        if String.length start > 3 then
          (* there is double digit number in start loc *)
          let start_loc = helper_double_digit (String.trim start) in
          if String.length end_ > 3 then
            (* double digit number in start and end loc *)
            (start_loc, helper_double_digit (String.trim end_))
            (* below : start is double digit, end is single digit *)
          else (start_loc, (end_.[1], int_of_char end_.[2] - 48))
        else
          (* start is single digit number *)
          let start_loc = (start.[0], int_of_char start.[1] - 48) in
          if String.length end_ > 3 then
            (start_loc, helper_double_digit (String.trim end_))
            (* start and end are both single digit number *)
          else (start_loc, (end_.[1], int_of_char end_.[2] - 48))
    | _ -> failwith "Not a valid location format!"

let rec tuple_list (letter_lst : string list) =
  match letter_lst with
  | [] -> []
  | h :: t ->
      (h.[0], int_of_string (String.sub h 1 (String.length h - 1)))
      :: tuple_list t

(* at least one of the words in each tuple needs to be a valid word. it doesn't
   matter if both of them are *)
let rec check_created_words (words : string list) : bool =
  match words with
  | [] -> true
  | h :: t -> if in_dictionary h then check_created_words t else false

(* the reverse of the empty string is the empty string; helper function used in
   (* board.ml *) let rec reverse_string x = match x with | "" -> "" | _ ->
   String.sub x (String.length x - 1) 1 ^ reverse_string (String.sub x 0
   (String.length x - 1)) *)

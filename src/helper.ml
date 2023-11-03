(*Contains all the helper methods that are used across multiple files*)

(* Given a list and an element, returns that list without the first appreance of
   that element. Helper function for play_tiles. *)
let rec list_without_elem (lst : 'a list) (elem : 'a) : 'a list =
  match lst with
  | [] -> lst
  | h :: t -> if h = elem then t else h :: list_without_elem t elem

(* Given a string [input], returns a char list representation of that string.
   Multipurpose helper function for *)
let rec char_list_of_string (input : string) : char list =
  match input with
  | "" -> []
  | str ->
      str.[0] :: char_list_of_string (String.sub str 1 (String.length str - 1))

(*Checks a given char list if the second given char list has all of it's
  elements in the first. Helper function uesd by check_word *)
let rec contains_chars (avail : char list) (used : char list) : bool =
  match used with
  | [] -> true
  | h :: t ->
      if List.find_opt (fun x -> if x = h then true else false) avail = None
      then false
      else contains_chars (list_without_elem avail h) t

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
  let file = "src/scrabble_dictionary.txt" in
  let dict = file |> In_channel.open_bin |> In_channel.input_all in
  let dict_lst = String.split_on_char '\n' dict in
  search_dict (String.uppercase_ascii input ^ "\r") dict_lst

(* Checks if the given [input] is in the scrabble dictionary, and if the tiles
   are in the player's hand. *)
let check_word (player_hand : char list) (input : string) : bool =
  let tiles_used = char_list_of_string input in
  contains_chars player_hand tiles_used && in_dictionary input

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
   bounds location. Currently, only works for 7x7 board. *)
let loc_in_bounds (loc : (char * int) * (char * int)) : bool =
  let fst_char = int_of_char (fst (fst loc)) in
  let snd_char = int_of_char (fst (snd loc)) in
  let fst_int = snd (fst loc) in
  let snd_int = snd (snd loc) in
  if
    fst_char > 70 || snd_char > 70 || fst_int > 7 || snd_int > 7 || fst_int < 1
    || snd_int < 1
  then false
  else true

let gen_loc (loc : string) =
  if loc = "" then (('a', -1), ('a', -1))
  else
    match
      loc |> String.split_on_char '-' |> List.filter (fun s -> s <> " ")
    with
    | [ start; end_ ] ->
        ( (start.[0], int_of_char start.[1] - 48),
          (end_.[1], int_of_char end_.[2] - 48) )
    | _ -> failwith "Not a valid location format!"

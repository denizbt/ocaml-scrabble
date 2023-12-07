(** The signature of the scrabble board. *)
module type BoardType = sig
  type tile
  type board_type
  type letter_bank
  type letter_points

  val init_board : int -> board_type
  val show_board : board_type -> unit
  val sample : int -> letter_bank -> char list

  val check_existence :
    string ->
    (char * int) * (char * int) ->
    board_type ->
    (char * (int * int)) list

  val add_word :
    string -> (char * int) * (char * int) -> board_type -> int -> unit

  val init_letter_bank : char list -> letter_bank
  val update_bank : letter_bank -> char list -> letter_bank
  val to_list_bank : letter_bank -> char list
  val init_letter_points : unit -> letter_points
  val letter_value : char -> letter_points -> int
  val calc_points : char list list -> letter_points -> int

  val calc_point_w_modifiers :
    char list list ->
    char list ->
    char list ->
    (int * int) list ->
    letter_points ->
    board_type ->
    int

  val created_words : board_type -> char list -> (int * int) list -> string list
end

(** Module representing a Scrabble board, and its letter bank. *)
module ScrabbleBoard : BoardType = struct
  (* Each tile in a board can either be Empty or contain a single char *)
  type tile =
    | Empty of string
    | Letter of char

  (* Board type is a mutable 2D array *)
  type board_type = tile array array

  (* Letter_bank is a multiset of the Scrabble letters *)
  type letter_bank = char list

  (* List of tuples where first element is the letter and the second element is
     the number of points which it is worth *)
  type letter_points = (char * int) list

  let init_board (n : int) : board_type =
    Array.of_list
      [
        Array.of_list
          [
            Empty "T"; Empty ""; Empty ""; Empty "d"; Empty ""; Empty "";
            Empty ""; Empty "T"; Empty ""; Empty ""; Empty ""; Empty "d";
            Empty ""; Empty ""; Empty "T";
          ];
        Array.of_list
          [
            Empty ""; Empty "D"; Empty ""; Empty ""; Empty ""; Empty "t";
            Empty ""; Empty ""; Empty ""; Empty "t"; Empty ""; Empty "";
            Empty ""; Empty "D"; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty "D"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "D"; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty "d"; Empty ""; Empty ""; Empty "D"; Empty ""; Empty "";
            Empty ""; Empty "d"; Empty ""; Empty ""; Empty ""; Empty "D";
            Empty ""; Empty ""; Empty "d";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty ""; Empty ""; Empty "D"; Empty "";
            Empty ""; Empty ""; Empty ""; Empty ""; Empty "D"; Empty "";
            Empty ""; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty "t"; Empty ""; Empty ""; Empty ""; Empty "t";
            Empty ""; Empty ""; Empty ""; Empty "t"; Empty ""; Empty "";
            Empty ""; Empty "t"; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty "T"; Empty ""; Empty ""; Empty "d"; Empty ""; Empty "";
            Empty ""; Empty "X"; Empty ""; Empty ""; Empty ""; Empty "d";
            Empty ""; Empty ""; Empty "T";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty "t"; Empty ""; Empty ""; Empty ""; Empty "t";
            Empty ""; Empty ""; Empty ""; Empty "t"; Empty ""; Empty "";
            Empty ""; Empty "t"; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty ""; Empty ""; Empty "D"; Empty "";
            Empty ""; Empty ""; Empty ""; Empty ""; Empty "D"; Empty "";
            Empty ""; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty "d"; Empty ""; Empty ""; Empty "D"; Empty ""; Empty "";
            Empty ""; Empty "d"; Empty ""; Empty ""; Empty ""; Empty "D";
            Empty ""; Empty ""; Empty "d";
          ];
        Array.of_list
          [
            Empty ""; Empty ""; Empty "D"; Empty ""; Empty ""; Empty "";
            Empty "d"; Empty ""; Empty "d"; Empty ""; Empty ""; Empty "";
            Empty "D"; Empty ""; Empty "";
          ];
        Array.of_list
          [
            Empty ""; Empty "D"; Empty ""; Empty ""; Empty ""; Empty "t";
            Empty ""; Empty ""; Empty ""; Empty "t"; Empty ""; Empty "";
            Empty ""; Empty "D"; Empty "";
          ];
        Array.of_list
          [
            Empty "T"; Empty ""; Empty ""; Empty "d"; Empty ""; Empty "";
            Empty ""; Empty "T"; Empty ""; Empty ""; Empty ""; Empty "d";
            Empty ""; Empty ""; Empty "T";
          ];
      ]

  let tile_to_string (currTile : tile) : string =
    match currTile with
    | Empty str -> (
        match str with
        | "" -> " - "
        | x -> "|" ^ x ^ "|")
    | Letter char -> " " ^ String.make 1 char ^ " "

  (*helper function to convert a letter to a number coordinate*)
  let position_of_char (letter : char) : int =
    match letter with
    | 'A' -> 0
    | 'B' -> 1
    | 'C' -> 2
    | 'D' -> 3
    | 'E' -> 4
    | 'F' -> 5
    | 'G' -> 6
    | 'H' -> 7
    | 'I' -> 8
    | 'J' -> 9
    | 'K' -> 10
    | 'L' -> 11
    | 'M' -> 12
    | 'N' -> 13
    | 'O' -> 14
    | _ -> failwith "invalid coordinate"

  (*helper function to convert a number coordinate to a letter*)
  let char_of_position (number : int) : char =
    match number with
    | 0 -> 'A'
    | 1 -> 'B'
    | 2 -> 'C'
    | 3 -> 'D'
    | 4 -> 'E'
    | 5 -> 'F'
    | 6 -> 'G'
    | 7 -> 'H'
    | 8 -> 'I'
    | 9 -> 'J'
    | 10 -> 'K'
    | 11 -> 'L'
    | 12 -> 'M'
    | 13 -> 'N'
    | 14 -> 'O'
    | _ -> failwith "invalid coordinate"

  let rec show_coordinates (board : board_type) (index : int) : string =
    if index < Array.length board - 1 then
      (" " ^ String.make 1 (char_of_position index) ^ " ")
      ^ show_coordinates board (index + 1)
    else " " ^ String.make 1 (char_of_position index) ^ " "

  let rec show_board_helper board (n : int) (m : int) : unit =
    if n >= Array.length board || m >= Array.length board then ()
    else if n = 0 then
      if m <= 9 then
        print_string (string_of_int m ^ "  " ^ tile_to_string board.(n).(m))
      else print_string (string_of_int m ^ " " ^ tile_to_string board.(n).(m))
    else if n + 1 = Array.length board then
      print_endline (tile_to_string board.(n).(m))
    else print_string (tile_to_string board.(n).(m));

    if n >= Array.length board then
      if m >= Array.length board then () else show_board_helper board 0 (m + 1)
    else show_board_helper board (n + 1) m

  (* Print asci representation of board to terminal*)
  let show_board (board : board_type) : unit =
    print_endline ("   " ^ show_coordinates board 0);
    show_board_helper board 0 0

  (** Given an integer, and the letter bank, returns a list of letters from the
      bank of length [count]. *)
  let rec sample_helper (count : int) (bank : char list) : char list =
    if count == 0 then []
    else (
      Random.self_init ();
      let n = Random.int (List.length bank) in
      let elem = List.nth bank n in
      elem :: sample_helper (count - 1) (Helper.list_without_elem bank elem))

  let sample (n : int) (bank : char list) : char list =
    match bank with
    | [] -> []
    | h :: t -> sample_helper n bank

  (* TO DO DELETE *)
  let pp_loc (loc : (char * int) * (char * int)) : string =
    let fst_char = String.make 1 (fst (fst loc)) in
    let snd_char = String.make 1 (fst (snd loc)) in
    let fst_int = string_of_int (snd (fst loc)) in
    let snd_int = string_of_int (snd (snd loc)) in
    fst_char ^ fst_int ^ " - " ^ snd_char ^ snd_int

  (* given a starting and ending coordinate for a location, returns the logical
     second coordinate (depending on whether it is vertical or horizontal)*)
  let update_location (location : (char * int) * (char * int)) :
      (char * int) * (char * int) =
    let starting = fst location in
    let ending = snd location in
    if fst starting = fst ending then ((fst starting, snd starting + 1), ending)
    else
      ( (char_of_position (position_of_char (fst starting) + 1), snd starting),
        ending )

  let rec get_bool (word : string) (index : int)
      (location : (char * int) * (char * int)) (board : board_type)
      (possible : bool) : bool =
    if index >= String.length word then possible
    else
      let col = position_of_char (fst (fst location)) in
      let row = snd (fst location) in
      let curr_tile = board.(col).(row) in
      (* print_endline (pp_loc location); print_string (string_of_int col ^ " "
         ^ string_of_int row ^ ", "); *)
      match curr_tile with
      | Empty _ ->
          get_bool word (index + 1) (update_location location) board true
      | Letter l ->
          if String.make 1 l = String.make 1 word.[index] then
            get_bool word (index + 1) (update_location location) board true
          else false

  let rec get_all_char (word : string) (index : int)
      (location : (char * int) * (char * int)) (board : board_type) :
      (char * (int * int)) list =
    if index >= String.length word then []
    else
      let col = position_of_char (fst (fst location)) in
      let row = snd (fst location) in
      let current = board.(col).(row) in
      if
        current = Empty "" || current = Empty "X" || current = Empty "D"
        || current = Empty "T" || current = Empty "t" || current = Empty "d"
      then
        (word.[index], (col, row))
        :: get_all_char word (index + 1) (update_location location) board
      else get_all_char word (index + 1) (update_location location) board

  (* Meant to account for letters already on the board (don't need to place over
     them and don't need to have letter in your hand) *)
  let check_existence (word : string) (location : (char * int) * (char * int))
      (board : board_type) : (char * (int * int)) list =
    let possible = get_bool word 0 location board true in
    if possible = true then get_all_char word 0 location board else []

  let rec add_word (word : string) (location : (char * int) * (char * int))
      (board : board_type) (index : int) : unit =
    let col = position_of_char (fst (fst location)) in
    let row = snd (fst location) in
    board.(col).(row) <- Letter word.[index];
    if index + 1 >= String.length word then ()
    else add_word word (update_location location) board (index + 1)

  (*helper function to get the word made by the tiles above the current tile*)
  let rec get_word_above (board : board_type) (location : int * int) : string =
    let x, y = (fst location, snd location) in
    if y >= 0 then
      (*upper left is 0, 0*)
      match board.(x).(y) with
      | Empty _ -> ""
      | Letter l -> get_word_above board (x, y - 1) ^ String.make 1 l
    else ""

  (*helper function to get the word made by tiles below the current tile*)
  let rec get_word_below (board : board_type) (location : int * int) : string =
    let x, y = (fst location, snd location) in
    if y <= Array.length board - 1 then
      match board.(x).(y) with
      | Empty _ -> ""
      | Letter l -> String.make 1 l ^ get_word_below board (x, y + 1)
    else ""

  (*helper function to get the word made by tiles to the left of the current
    tile*)
  let rec get_word_left (board : board_type) (location : int * int) : string =
    let x, y = (fst location, snd location) in
    if x > 0 then
      match board.(x - 1).(y) with
      | Empty _ -> ""
      | Letter curr -> get_word_left board (x - 1, y) ^ String.make 1 curr
    else ""

  (*helper function to get the word made by tiles to the right of the current
    tile*)
  let rec get_word_right (board : board_type) (location : int * int) : string =
    let x, y = (fst location, snd location) in
    if x < Array.length board - 1 then
      match board.(x + 1).(y) with
      | Empty _ -> ""
      | Letter curr -> String.make 1 curr ^ get_word_right board (x + 1, y)
    else ""

  let update_location_int (cur_loc : int * int) (end_loc : int * int) :
      int * int =
    if fst cur_loc = fst end_loc then (fst cur_loc, snd cur_loc + 1)
    else (fst cur_loc + 1, snd cur_loc)

  let rec iterate_board (board : board_type) (cur_loc : int * int)
      (end_loc : int * int) (word_index : int) (word : char list) : string =
    if fst cur_loc = fst end_loc && snd cur_loc = snd end_loc then
      match board.(fst cur_loc).(snd cur_loc) with
      | Empty _ -> String.make 1 (List.nth word word_index)
      | Letter t -> String.make 1 t
    else
      match board.(fst cur_loc).(snd cur_loc) with
      | Empty _ ->
          String.make 1 (List.nth word word_index)
          ^ iterate_board board
              (update_location_int cur_loc end_loc)
              end_loc (word_index + 1) word
      | Letter t ->
          String.make 1 t
          ^ iterate_board board
              (update_location_int cur_loc end_loc)
              end_loc word_index word

  (* Only call horizontal_checker on words which are horizontal *)
  let rec horizontal_checker (board : board_type) (word : char list)
      (location : (int * int) list) : string =
    let start_loc = List.hd location in
    let end_loc = List.nth location (List.length location - 1) in
    let word_left = get_word_left board start_loc in
    let word_right = get_word_right board end_loc in
    let word_middle = iterate_board board start_loc end_loc 0 word in
    print_endline (word_left ^ " " ^ word_middle ^ " " ^ word_right);
    word_left ^ word_middle ^ word_right

  (* Only call vertical_checker on words which are vertical *)
  let rec vertical_checker (board : board_type) (word : char list)
      (location : (int * int) list) : string =
    let start_loc = List.hd location in
    let end_loc = List.nth location (List.length location - 1) in
    let word_up = get_word_above board (fst start_loc, snd start_loc - 1) in
    let word_down = get_word_below board (fst end_loc, snd end_loc + 1) in
    let word_middle = iterate_board board start_loc end_loc 0 word in
    print_endline (word_up ^ " " ^ word_middle ^ " " ^ word_down);
    word_up ^ word_middle ^ word_down

  (** Given a board [board_type], letters you want to add (must not be on the
      board already), and the location of where you want to add the new letters
      to the board, return a list of all possible words that could be created
      from words already places surrouning the new word. *)

  (*apply horizontal checker to every index of a word (intended for the body of
    a vertical word)*)
  let rec horizontal_helper (board : board_type) (word : char list)
      (location : (int * int) list) (index : int) : string list =
    if index = List.length word then []
    else
      let created =
        horizontal_checker board
          [ List.nth word index ]
          [ List.nth location index ]
      in
      if String.length created = 1 then
        horizontal_helper board word location (index + 1)
      else created :: horizontal_helper board word location (index + 1)

  (*apply vertical checker to every index of a word (intended for the body of a
    horizontal word)*)
  let rec vertical_helper (board : board_type) (word : char list)
      (location : (int * int) list) (index : int) : string list =
    if index = List.length word then []
    else
      let created =
        vertical_checker board
          [ List.nth word index ]
          [ List.nth location index ]
      in
      if String.length created = 1 then
        vertical_helper board word location (index + 1)
      else created :: vertical_helper board word location (index + 1)

  (*helper function to remove a word from a list if it exists in the list, else
    return the original list*)
  let rec remove_element (word : string) (lst : string list) : string list =
    match lst with
    | [] -> []
    | h :: t ->
        if h = word then remove_element word t else h :: remove_element word t

  (*helper function to turn a char list to a string*)
  let rec char_list_to_string (lst : char list) : string =
    match lst with
    | [] -> ""
    | h :: t -> String.make 1 h ^ char_list_to_string t

  let created_words (board : board_type) (word : char list)
      (location : (int * int) list) : string list =
    if List.length location <> List.length word then
      failwith "pre-condition violated in created_words"
    else
      (* first element of tuple is index of column!!! *)
      let direction =
        match location with
        | [] -> failwith "created_words: no new letters added to board!"
        | h1 :: h2 :: t ->
            if fst h1 = fst h2 then "vertical"
            else if snd h1 = snd h2 then "horizontal"
            else failwith "not valid location"
        | h :: t -> "one letter"
      in
      let word_s = char_list_to_string word in
      if direction = "horizontal" then
        remove_element word_s
          ([ horizontal_checker board word location ]
          @ vertical_helper board word location 0)
      else if direction = "vertical" then
        remove_element word_s
          ([ vertical_checker board word location ]
          @ horizontal_helper board word location 0)
      else if direction = "one letter" then
        remove_element word_s
          ([ horizontal_checker board word location ]
          @ [ vertical_checker board word location ])
      else failwith "smth went wrong in created_words; should never be here"

  (* Letter Bank functions *)

  (* If input is [], then the official English Scrabble letter bank is read in
     from file scrabble_letter_bank.txt. Otherwise, the given list is used as a
     letter bank typed variable. *)
  let init_letter_bank (input : char list) : letter_bank =
    match input with
    | [] ->
        "src/scrabble_letter_bank.txt" |> In_channel.open_text
        |> In_channel.input_all |> Helper.char_list_of_string
    | _ -> input

  let rec update_bank (bank : letter_bank) (sampled : char list) : letter_bank =
    match sampled with
    | [] -> bank
    | h :: t ->
        let x = List.find_opt (fun x -> if x = h then true else false) bank in
        if x = None then update_bank bank t
        else update_bank (Helper.list_without_elem bank (Option.get x)) t

  let to_list_bank (bank : letter_bank) = bank

  let init_letter_points () : letter_points =
    "src/letter_points.txt" |> In_channel.open_text |> In_channel.input_all
    |> String.split_on_char ' ' |> Helper.tuple_list

  let letter_value (letter : char) (letter_points : letter_points) : int =
    List.assoc letter letter_points

  (* Helper function which calculates points in given word *)
  let rec calc_word_pts (word : char list) (letter_points : letter_points) : int
      =
    match word with
    | [] -> 0
    | h :: t -> List.assoc h letter_points + calc_word_pts t letter_points

  (*Given inputted word as a char list, the location of the added tiles, returns
    all the tiles of the new letters on the board*)
  let rec get_added_tiles (inputted_word : char list) (locs : (int * int) list)
      (board : board_type) : tile list =
    match locs with
    | [] -> []
    | h :: t ->
        let row = Array.get board (fst h) in
        Array.get row (snd h) :: get_added_tiles inputted_word t board

  (*returns the modified score of inputted word*)
  let check_modifiers (inputted_word : char list) (old_tiles : char list)
      (locs : (int * int) list) (board : board_type)
      (letter_points : letter_points) : int =
    let added_tiles = get_added_tiles inputted_word locs board in
    let rec letter_mult (lst, inputted_word) : int =
      match (lst, inputted_word) with
      | h1 :: t1, h2 :: t2 -> (
          match h1 with
          | Empty "d" ->
              (letter_value h2 letter_points * 2) + letter_mult (t1, t2)
          | Empty "t" ->
              (letter_value h2 letter_points * 3) + letter_mult (t1, t2)
          | _ -> letter_value h2 letter_points + letter_mult (t1, t2))
      | _ -> 0
    in
    let rec word_mult tiles : int =
      match tiles with
      | [] -> 1
      | h :: t -> (
          match h with
          | Empty "D" -> 2 * word_mult t
          | Empty "T" -> 3 * word_mult t
          | _ -> word_mult t)
    in
    (calc_word_pts old_tiles letter_points
    + letter_mult (added_tiles, inputted_word))
    * word_mult added_tiles

  (* Requires that every char in element of [words] is in letter_points. *)
  let rec calc_points (words : char list list) (letter_points : letter_points) :
      int =
    match words with
    | [] -> 0
    | h :: t -> calc_word_pts h letter_points + calc_points t letter_points

  let rec list_difference (og : 'a list) (diff : 'a list) : 'a list =
    match diff with
    | [] -> og
    | h :: t -> list_difference (Helper.list_without_elem og h) t

  (*Updates the score with the score updated by modifiers*)
  let calc_point_w_modifiers (words : char list list) (input : char list)
      (new_tiles : char list) (locs : (int * int) list)
      (letter_points : letter_points) (board : board_type) : int =
    let prev_calc = calc_points words letter_points in
    let c_w_p = calc_word_pts input letter_points in
    let c_m =
      check_modifiers input
        (list_difference input new_tiles)
        locs board letter_points
    in
    prev_calc - c_w_p + c_m
end

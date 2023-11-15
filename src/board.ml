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
    string -> (char * int) * (char * int) -> board_type -> char list

  val add_word :
    string -> (char * int) * (char * int) -> board_type -> int -> unit

  val init_letter_bank : char list -> letter_bank
  val update_bank : letter_bank -> char list -> letter_bank
  val to_list_bank : letter_bank -> char list
  val init_letter_points : unit -> letter_points
  val letter_value : char -> letter_points -> int
  val calc_points : char list list -> letter_points -> int

  val created_words :
    board_type ->
    string ->
    (char * int) * (char * int) ->
    (string * string) list
end

(** Module representing a Scrabble board, and its letter bank. *)
module ScrabbleBoard : BoardType = struct
  (* Each tile in a board can either be Empty or contain a single char *)
  type tile =
    | Empty
    | Letter of char

  (* Board type is a mutable 2D array *)
  type board_type = tile array array

  (* Letter_bank is a multiset of the Scrabble letters *)
  type letter_bank = char list

  (* List of tuples where first element is the letter and the second element is
     the number of points which it is worth *)
  type letter_points = (char * int) list

  let init_board (n : int) : board_type = Array.make_matrix n n Empty

  let tile_to_string (currTile : tile) : string =
    match currTile with
    | Empty -> " - "
    | Letter char -> " " ^ String.make 1 char ^ " "

  (* Helper function to convert a letter to a number coordinate BUT ONLY UP TO
     7X7*)
  let position_of_char (letter : char) : int =
    match letter with
    | 'A' -> 1
    | 'B' -> 2
    | 'C' -> 3
    | 'D' -> 4
    | 'E' -> 5
    | 'F' -> 6
    | 'G' -> 7
    | _ -> failwith "invalid coordinate"

  (*helper function to convert a number coordinate to a letter BUT ONLY UP TO
    7X7*)
  let char_of_position (number : int) : char =
    match number with
    | 1 -> 'A'
    | 2 -> 'B'
    | 3 -> 'C'
    | 4 -> 'D'
    | 5 -> 'E'
    | 6 -> 'F'
    | 7 -> 'G'
    | _ -> failwith "invalid coordinate"

  let rec show_coordinates (board : board_type) (index : int) : string =
    if index < Array.length board - 2 then
      (" " ^ String.make 1 (char_of_position (index + 1)) ^ " ")
      ^ show_coordinates board (index + 1)
    else " " ^ String.make 1 (char_of_position (index + 1)) ^ " "

  let rec show_board_helper board (n : int) (m : int) : unit =
    if n >= Array.length board || m >= Array.length board then ()
    else if n = 0 then print_string (string_of_int (m + 1) ^ " ")
    else if n + 1 = Array.length board then
      print_endline (tile_to_string board.(n).(m))
    else print_string (tile_to_string board.(n).(m));

    if n >= Array.length board then
      if m >= Array.length board then () else show_board_helper board 0 (m + 1)
    else show_board_helper board (n + 1) m

  (*print asci representation of board to terminal*)
  let show_board (board : board_type) : unit =
    print_endline ("  " ^ show_coordinates board 0);
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

  (*helper function to convert a letter to a number coordinate*)
  let position_of_char (letter : char) : int =
    match letter with
    | 'A' -> 1
    | 'B' -> 2
    | 'C' -> 3
    | 'D' -> 4
    | 'E' -> 5
    | 'F' -> 6
    | 'G' -> 7
    | _ -> failwith "invalid coordinate"

  (*helper function to convert a number coordinate to a letter*)
  let char_of_position (number : int) : char =
    match number with
    | 1 -> 'A'
    | 2 -> 'B'
    | 3 -> 'C'
    | 4 -> 'D'
    | 5 -> 'E'
    | 6 -> 'F'
    | 7 -> 'G'
    | _ -> failwith "invalid coordinate"

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
    if index + 1 >= String.length word then possible
    else
      let current =
        board.(position_of_char (fst (fst location))).(snd (fst location) - 1)
      in
      if current = Empty || current = Letter word.[index] then
        get_bool word (index + 1) (update_location location) board true
      else false

  let rec get_all_char (word : string) (index : int)
      (location : (char * int) * (char * int)) (board : board_type) : char list
      =
    if index >= String.length word then []
    else
      let current =
        board.(position_of_char (fst (fst location))).(snd (fst location) - 1)
      in
      if current = Empty then
        word.[index]
        :: get_all_char word (index + 1) (update_location location) board
      else get_all_char word (index + 1) (update_location location) board

  let check_existence (word : string) (location : (char * int) * (char * int))
      (board : board_type) : char list =
    let possible = get_bool word 0 location board true in
    if possible = true then get_all_char word 0 location board else []

  let rec add_word (word : string) (location : (char * int) * (char * int))
      (board : board_type) (index : int) : unit =
    board.(position_of_char (fst (fst location))).(snd (fst location) - 1) <-
      Letter word.[index];
    if index + 1 >= String.length word then ()
    else add_word word (update_location location) board (index + 1)

  (*helper function to get the word made by the tiles above the current tile*)
  let rec get_word_above (board : board_type) (location : char * int) : string =
    let x, y = (position_of_char (fst location), snd location - 1) in
    if y >= 0 then
      match board.(x).(y) with
      | Empty -> ""
      | Letter x -> get_word_above board (fst location, y) ^ String.make 1 x
    else ""

  (*helper function to get the word made by tiles below the current tile*)
  let rec get_word_below (board : board_type) (location : char * int) : string =
    let x, y = (position_of_char (fst location), snd location) in
    if y <= Array.length board - 1 then
      match board.(x).(y) with
      | Empty -> ""
      | Letter x -> String.make 1 x ^ get_word_below board (fst location, y + 1)
    else ""

  (*helper function to get the word made by tiles to the left of the current
    tile*)
  let rec get_word_left (board : board_type) (location : char * int) : string =
    let x, y = (position_of_char (fst location), snd location) in

    if x > 0 then
      match board.(x - 1).(y) with
      | Empty -> ""
      | Letter curr ->
          get_word_left board (char_of_position (x - 1), y) ^ String.make 1 curr
    else ""

  (*helper function to get the word made by tiles to the right of the current
    tile*)
  let rec get_word_right (board : board_type) (location : char * int) : string =
    let x, y = (position_of_char (fst location), snd location) in

    if x < Array.length board - 1 then
      match board.(x + 1).(y) with
      | Empty -> ""
      | Letter curr ->
          String.make 1 curr ^ get_word_right board (char_of_position (x + 1), y)
    else ""

  (*helper function to get the word made by tiles to the right and left of the
    current tile*)
  let init_horizontal_helper (board : board_type) (word : string)
      (location : (char * int) * (char * int)) : string =
    let first =
      get_word_left board (fst (fst location), snd (fst location) - 1)
    in
    let second =
      get_word_right board (fst (snd location), snd (snd location) - 1)
    in

    first ^ word ^ second

  (*helper function to get all of the words made by tiles above and below the
    current tile*)
  let init_vertical_helper (board : board_type) (word : string)
      (location : (char * int) * (char * int)) : string =
    let first =
      get_word_above board (fst (fst location), snd (fst location) - 1)
    in
    let second =
      get_word_below board (fst (snd location), snd (snd location))
    in
    if first = "" && second = "" then "" else first ^ word ^ second
  (*helper function to traverse through the rest of a word and get words made up
    of the surrounding tiles (excluding the ends)*)

  let rec created_words_helper (board : board_type) (word : string)
      (location : (char * int) * (char * int)) (index : int) (vertical : bool) :
      string list =
    if index <= String.length word - 1 then
      let curr =
        if vertical (*vertical*) then
          [ init_horizontal_helper board (String.make 1 word.[index]) location ]
        else
          (*horizontal*)
          [ init_vertical_helper board (String.make 1 word.[index]) location ]
      in
      curr
      @ created_words_helper board word (update_location location) (index + 1)
          vertical
    else []

  (*the final list of all the created words (getting rid of empty string) as
    tuples, the first element being how it originally was and the second element
    being the same word reversed*)

  let rec all_created_words (lst : string list) : (string * string) list =
    match lst with
    | [] -> []
    | h :: t ->
        if h = "" then all_created_words t
        else (h, Helper.reverse_string h) :: all_created_words t

  (** given a board [board_type], letters you want to add (must not be on the
      board already), and the location of where you want to add the new letters
      to the board, return a list of all possible words that could be created
      from words already places surrouning the new word*)
  let created_words (board : board_type) (word : string)
      (location : (char * int) * (char * int)) : (string * string) list =
    let lst =
      if (*only one letter*)
         String.length word = 1 then
        [
          init_vertical_helper board word location;
          init_horizontal_helper board word location;
        ] (*vertical*)
      else if fst (fst location) = fst (snd location) then
        init_vertical_helper board word location
        :: created_words_helper board word location 0 true (*horizontal*)
      else
        init_horizontal_helper board word location
        :: created_words_helper board word location 0 false
    in
    all_created_words lst

  (* Letter Bank functions *)

  (* If input is [], then the official English Scrabble letter bank is read in
     from file scrabble_letter_bank.txt. Otherwise, the given list is used as a
     letter bank typed variable. *)
  let init_letter_bank (input : char list) : letter_bank =
    match input with
    | [] ->
        "run/scrabble_letter_bank.txt" |> In_channel.open_text
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

  (* Requires that every char in [word] is in letter_points. *)
  let rec calc_points (words : char list list) (letter_points : letter_points) :
      int =
    match words with
    | [] -> 0
    | h :: t -> calc_word_pts h letter_points + calc_points t letter_points
end

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
  val calc_points : char list -> letter_points -> int
end

(** Module representing a Scrabble board. *)
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

  (*given word, location, and board, returns a list of tiles the player must
    have. if the word cannot be put there, return the empty list. meant to
    account for letters already on the board (don't need to place over them and
    don't need to have letter in your hand)*)
  let check_existence (word : string) (location : (char * int) * (char * int))
      (board : board_type) : char list =
    let possible = get_bool word 0 location board true in
    if possible = true then get_all_char word 0 location board else []

  let rec add_word (word : string) (location : (char * int) * (char * int))
      (board : board_type) (index : int) =
    board.(position_of_char (fst (fst location))).(snd (fst location) - 1) <-
      Letter word.[index];
    if index + 1 >= String.length word then ()
    else add_word word (update_location location) board (index + 1)

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

  (* Requires that every char in [word] is in letter_points. *)
  let rec calc_points (word : char list) (letter_points : letter_points) : int =
    match word with
    | [] -> 0
    | h :: t -> List.assoc h letter_points + calc_points t letter_points
end

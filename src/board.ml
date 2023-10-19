(** The signature of the scrabble board. *)
module type BoardType = sig
  type letter_bank
  type tile
  type board_type

  val init_board : int -> int -> board_type
  val show_board : unit

  (* val check_word_fit : string -> bool *)
  (** Check if given String [word], fits on the current [board] *)

  val sample : int -> letter_bank -> tile list

  val add_word :
    string -> (char * int) * (char * int) -> board_type -> int -> board_type
end

(** Scrabble board *)
module ScrabbleBoard : BoardType = struct
  (* letter_bank is a multiset of the Scrabble letters *)
  type letter_bank = char list

  (* Each tile in a board *)
  type tile =
    | Empty
    | Letter of char

  type board_type = tile array array

  let init_board (w : int) (h : int) : board_type = Array.make_matrix w h Empty
  (* Creates a 2D array which represents the board. *)

  let show_board = ()

  (* Given a list and an element, returns that list without the first appreance
     of that element. Helper function for play_tiles *)
  let rec list_without_elem (lst : 'a list) (elem : 'a) : 'a list =
    match lst with
    | [] -> lst
    | h :: t -> if h = elem then t else h :: list_without_elem t elem

  (* Given an integer, and the letter bank, returns *)
  let rec sample_helper (count : int) (bank : letter_bank) : tile list =
    if count == 0 then []
    else
      let n = Random.int (List.length bank) in
      let elem = List.nth bank n in
      Letter elem :: sample_helper (count - 1) (list_without_elem bank elem)

  let sample (n : int) (bank : letter_bank) : tile list =
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

  let rec valid_dir (starter : char * int) (ending : char * int) : bool =
    if fst starter = fst ending then true
    else if snd starter = snd ending then true
    else false

  (*TODO: Implement later!!! setting to true rn to make addword work. also check
    word is not empty*)
  let check_word_fit (word : string) (location : (char * int) * (char * int)) =
    let starting = fst location in
    let ending = snd location in
    if valid_dir starting ending then
      if fst starting = fst ending then
        snd ending - snd starting >= String.length word
      else
        position_of_char (fst ending) - position_of_char (fst starting)
        >= String.length word
    else false

  let update_loction (location : (char * int) * (char * int)) :
      (char * int) * (char * int) =
    let starting = fst location in
    let ending = snd location in
    if fst starting = fst ending then ((fst starting, snd starting + 1), ending)
    else
      ( (char_of_position (position_of_char (fst starting) + 1), snd starting),
        ending )

  (** Given list of chars representing the word, and a tuple of the starting and
      ending location of word on the board. *)
  let rec add_word (word : string) (location : (char * int) * (char * int))
      (board : board_type) (index : int) : board_type =
    if check_word_fit word location then
      board.(position_of_char (fst (fst location))).(snd (fst location)) <-
        Letter word.[index];
    let new_location = update_loction location in
    add_word
      (String.sub word (index + 1) (String.length word))
      new_location board (index + 1)
end

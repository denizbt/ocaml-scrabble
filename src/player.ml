open Board

module type PlayerType = sig
  type t

  val create : char list -> t
  val current_tiles : t -> char list
  val score : t -> int
  val make_play : t -> string -> char * int -> char * int -> bool
end

module SinglePlayer : PlayerType = struct
  module ScrabbleBoard : BoardType = ScrabbleBoard

  type t = {
    score : int;
    tiles : char list;
  }
  (** Representation type of the model. *)

  let create (lst : char list) : t = { score = 0; tiles = lst }
  (*creates a player given a list of tile letters they have*)

  (** Returns their current tiles *)
  let current_tiles (player : t) : char list = player.tiles

  (** Returns their current score*)
  let score (player : t) : int = player.score

  (*Converts a string to a char list. Helper function used in check_word and
    make_play*)
  let rec string_to_char_list (input : string) : char list =
    match input with
    | "" -> []
    | x ->
        String.get x 0
        :: string_to_char_list (String.sub x 1 (String.length x - 1))

  (*Checks a given char list if the second given char list has all of it's
    elements in the first. Helper function uesd by check_word *)
  let rec contains_chars (avail : char list) (used : char list) : bool =
    match used with
    | [] -> true
    | h :: t ->
        if List.find_opt (fun x -> if x = h then true else false) avail = None
        then false
        else
          contains_chars
            (List.filter (fun x -> if x = h then false else true) avail)
            t

  (*Given an inputted string and a string list dictionary, checks the dictionary
    if the input is in there*)
  let rec search_dict (input : string) (dict_lst : string list) : bool =
    match dict_lst with
    | [] -> false
    | h :: t ->
        if String.get h 0 > String.get input 0 then false
        else if h = input then true
        else search_dict input t

  (*Creates dictionary as a string list and returns whether the word is in the
    dictionary. Helper function used in check_word*)
  let in_dictionary (input : string) : bool =
    let file = "scrabble_dictionary.txt" in
    let dict = file |> In_channel.open_text |> In_channel.input_all in
    let dict_lst = String.split_on_char '\n' dict in
    search_dict input dict_lst

  (*Verifies that the inputted starting position and ending position of an
    inputted word is possible / in vertical or horizontal direction. Helper
    function used in check_word*)
  let rec valid_dir (starter : char * int) (ending : char * int) : bool =
    if fst starter = fst ending then true
    else if snd starter = snd ending then true
    else false

  (*Checks if starting and ending is in valid direction (horizontal and
    vertical), if the word is in the dictionary, and if the tiles are in the
    user's hand (TEMP)*)
  let check_word (player : t) (input : string) (starter : char * int)
      (ending : char * int) : bool =
    let tiles_avail = player.tiles in
    let tiles_used = string_to_char_list input in
    contains_chars tiles_avail tiles_used
    && valid_dir starter ending && in_dictionary input

  (* Given a list and an element, returns that list without the first appreance
     of that element. Helper function for play_tiles *)
  let rec list_without_elem (lst : 'a list) (elem : 'a) : 'a list =
    match lst with
    | [] -> lst
    | h :: t -> if h = elem then t else h :: list_without_elem t elem

  (*Given the tiles and the input, returns the new tiles without 1 of each of
    the input. Helper function for make_play*)
  let rec update_tiles (tiles : char list) (input : char list) : char list =
    match input with
    | [] -> []
    | h :: t ->
        let x = List.find_opt (fun x -> if x = h then true else false) tiles in
        if x = None then h :: update_tiles tiles t
        else update_tiles (list_without_elem tiles (Option.get x)) t

  (** Given word, starting position, and ending position, sends the information
      to the board to be played. *)
  let make_play (player : t) (input : string) (starter : char * int)
      (ending : char * int) : bool =
    if check_word player input starter ending then
      if ScrabbleBoard.add_word input (starter, ending) then
        player.tiles = update_tiles player.tiles (string_to_char_list input)
      else false
    else false

  (*Given an non-negative integer n, draws n tiles from tile bag which is
    associated with the board, and return the updated player with updated
    tiles*)
  let draw_tiles (player : t) (n : int) : t =
    { score = player.score; tiles = player.tiles @ ScrabbleBoard.sample n }
end

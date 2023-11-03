(* Given a list and an element, returns that list without the first appreance of
   that element. Helper function for play_tiles. *)
val list_without_elem : 'a list -> 'a -> 'a list

(* Given a string [input], returns a char list representation of that string.
   Multipurpose helper function for *)
val char_list_of_string : string -> char list

(* Creates dictionary as a string list and returns whether the word is in the
   dictionary. Helper function used in check_word*)
val in_dictionary : string -> bool

(* Checks if the given [input] is in the scrabble dictionary, and if the tiles
   are in the player's hand. *)
val check_word : char list -> string -> bool

val valid_dir : (char * int) * (char * int) -> bool
(** Verifies that the inputted starting position and ending position of an
    inputted word is in vertical or horizontal direction, and goes from left to
    right OR up to down. *)

val valid_loc_length : (char * int) * (char * int) -> string -> bool
(** Given [loc], checks whether the number of spots defined by location is the
    right length for the word. Requires that location is in bounds (i.e.
    loc_in_bounds loc = true) *)

val loc_in_bounds : (char * int) * (char * int) -> bool
(** Given tuple of location, checks that the characters locations are in the
    range [A,F] and numbers are in the range [1,7]. *)

val gen_loc : string -> (char * int) * (char * int)
(** Given string of location (user input), returns a tuple of tuple
    representation of the location. Requires that the format is exactly similar
    "A1 - A4" with exactly 1 space surrounding the hyphen and no extra spaces
    anywhere. *)

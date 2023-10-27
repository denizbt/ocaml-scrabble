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

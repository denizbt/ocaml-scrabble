val list_without_elem : 'a list -> 'a -> 'a list
(** Given a list and an element, returns that list without the first appreance
    of that element. Helper function for play_tiles. *)

val char_list_of_string : string -> char list
(** Given a string [input], returns a char list representation of that string.
    Multipurpose helper function for *)

val string_of_char_list : char list -> string
(** Given a char list, returns a string which is the concatenation of every char
    in the lst in order. *)

val create_dictionary : string list
(** Creates a string list implementation of the scrabble dictionary*)

val in_dictionary : string -> bool
(** Creates dictionary using create_dictionary and returns whether the word is
    in the dictionary.*)

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

val tuple_list : string list -> (char * int) list
(** Given a string list, returns a list of tuples. First element of tuple is
    first character of string, second element of tuple is the rest of the string
    converted to int. Requires that the string [1, length-1] substring is
    convertable to an integer. *)

val check_created_words : (string * string) list -> bool
(** Given [lst], a of pairs of words (the tuple contains two strings, one of
    which is the reverse of the other), checks to make sure that every word (in
    either reverse form or not) in [lst] is valid according to in_dictionary. *)

val reverse_string : string -> string
(** Given a string, returns the string with its characters in reverse order. For
    example, reverse_string "HELLO" returns "OLLEH". The reverse of the empty
    string is the empty string. *)

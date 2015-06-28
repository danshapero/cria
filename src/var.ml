(** Note: at some point we need to make this a record type and include
    information about where the identifier was defined, either in the source
    code or the top-level environment. *)
type t = string

let count = ref 0
let gen_var s =
  incr count;
  s ^ "#" ^ (string_of_int !count)

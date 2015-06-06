
type data_type =
  | Nil_t
  | Bool_t
  | Int_t
  | Float_t
  | Function_t of (data_type list) * data_type

let rec string_of_data_type t =
  match t with
  | Nil_t -> "nil"
  | Bool_t -> "bool"
  | Int_t -> "int"
  | Float_t -> "float"
  | Function_t (args, ret) ->
     let args_string =
       String.concat " " (List.map string_of_data_type args)
     and ret_string =
       string_of_data_type ret
     in
     "(-> " ^ args_string ^ " " ^ ret_string ^ ")"


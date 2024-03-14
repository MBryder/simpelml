type t =
  | EOF
  | Invalid
  | Const
  | Mut
  | Ident of string
  | Assign
  | Eq
  | Int of int
  | Semicolon
  | AlphaPrint

let to_string t =
  match t with
  | EOF -> "EOF"
  | Invalid -> "Invalid"
  | Const -> "Const"
  | Mut -> "Mut"
  | Ident s -> "Ident " ^ "'" ^ s ^ "'"
  | Int i -> "Int " ^ "'" ^ Int.to_string i ^ "'"
  | Assign -> "Assign"
  | Eq -> "Eq"
  | Semicolon -> ";"
  | AlphaPrint -> "AlphaPrint"

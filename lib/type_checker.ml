(* udkast den til den flotte typechecker*)
(* types on god*)

(* Type definitions *)
type expr =
  | Int of int
  | Var of string

type typ =
  | TInt
  | TError of string

(* Environment to store variable types *)
type env = (string, typ) Hashtbl.t

exception TypeError of string

(* Initialize the environment *)
let initialize_env () =
  let env = Hashtbl.create ~random:true 10 in
  Hashtbl.add env "x" TInt;  (* Assuming 'x' is an integer *)
  env

(* Type checking function *)
let rec type_of_expr env = function
  | Int _ -> TInt  (* Integer literals are always of type TInt *)
  | Var x ->
      (try Hashtbl.find env x
       with Not_found -> raise (TypeError ("Unbound variable: " ^ x)))

(* Example usage *)
let _ =
  let env = initialize_env () in
  let expr = Var "x" in
  let expr_type = type_of_expr env expr in
  match expr_type with
  | TInt -> print_endline "The expression is an integer."
  | TError msg -> print_endline ("Type error: " ^ msg)

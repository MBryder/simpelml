(* udkast den til den flotte typechecker*)
(* types on god*)

(* Type definitions *)
type expr =
  | Int of int
  | Float of float
  | Var of string

type typ =
  | TInt
  | TFloat 
  | TError of string

(* Environment to store variable types *)
type env = (string, typ) Hashtbl.t

exception TypeError of string

(* Initialize the environment *)
let initialize_env () =
  let env = Hashtbl.create ~random:true 10 in
  Hashtbl.add env "x" TInt;  (* Assuming 'x' is an integer *)
  Hashtbl.add env "y" TFloat; (*og y er en float heheh*)
  env

(* Type checking function *)
let rec type_of_expr env = function
  | Int _ -> TInt  (* Integer literals are always of type TInt *)
  | Float _ -> TFloat (* and samesies with float*)
  | Var x ->
      (try Hashtbl.find env x
       with Not_found -> raise (TypeError ("Unbound variable: " ^ x)))

(* Example usage *)

let _ =
  let env = initialize_env () in
  let expr1 = Var "x" in
  let expr2 = Var "y" in 
  let expr1_type = type_of_expr env expr1 in
  let expr2_type = type_of_expr env expr2 in

  match expr1_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TError msg -> print_endline ("Type error: " ^ msg); 

  match expr2_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TError msg -> print_endline ("Type error: " ^ msg) 














(*let _ =
  let env = initialize_env () in
  let expr1 = Var "x" in
  let expr2 = Var "y" in 
  let expr1_type = type_of_expr env expr1 in
  let expr2_type = type_of_expr env expr2 in

  match expr2_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TError msg -> print_endline ("Type error: " ^ msg); 

  match expr1_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TError msg -> print_endline ("Type error: " ^ msg)*)
  


















  (*(* Example usage *)
let _ =
  let env = initialize_env () in
  let expr1 = Var "x" in
  let expr2 = Var "y" in 
  let expr1_type = type_of_expr env expr1 in
  let expr2_type = type_of_expr env expr2 in
  match expr1_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  (*| TInt, TFloat | TFloat, TInt -> print_endline "There's been a mix up between an integer and a float."
  | TInt, TFloat -> print_endline "There's been a mix up between an integer and a float"
  | TFloat, TInt -> print_endline "There's been a mix up between a float and an integer."*)
  | TError msg -> print_endline ("Type error: " ^ msg)
  (*| TError msg, _ | _, TError msg -> print_endline ("Type error: " ^ msg)*) *)
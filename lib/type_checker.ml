(* udkast den til den flotte typechecker*)
(* types on god*)

(* Type definitions *)
type expr =
  | Int of int
  | Float of float
  | Bool of bool 
  | String of string 
  | Var of string

type typ =
  | TInt
  | TFloat 
  | TBool 
  | TString 
  | TError of string

(* Environment to store variable types *)
type env = (string, typ) Hashtbl.t

exception TypeError of string

(* Initialize the environment *)
let initialize_env () =
  let env = Hashtbl.create ~random:true 10 in
  Hashtbl.add env "a" TInt;  (* Assuming 'a' is an integer *)
  Hashtbl.add env "b" TFloat; (*og b er en float heheh*)
  Hashtbl.add env "c" TBool;
  Hashtbl.add env "d" TString;

  env

(* Type checking function *)
let rec type_of_expr env = function
  | Int _ -> TInt  (* Integer literals are always of type TInt *)
  | Float _ -> TFloat (* and samesies with float*)
  | Bool _  -> TBool
  | String _ -> TString
  | Var x ->
      (try Hashtbl.find env x
       with Not_found -> raise (TypeError ("Unbound variable: " ^ x)))

(* Example usage *)

let _ =
  let env = initialize_env () in
  let exprInt = Var "a" in (* int *)
  let exprFloat = Var "b" in (* float *)
  let exprBool = Var "c" in (* bool *)
  let exprString = Var "d" in (* string *)
  let exprInt_type = type_of_expr env exprInt in (* Her kan man få warning 26 hvis der ikke er et match this with denne, det er ikke farligt wink wink altså det kan godt køre*)
  let exprFloat_type = type_of_expr env exprFloat in
  let exprBool_type = type_of_expr env exprBool in
  let exprString_type = type_of_expr env exprString in


  match exprString_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TBool -> print_endline "The expression is an bool."
  | TString -> print_endline "The expression is an string."
  | TError msg -> print_endline ("Type error: " ^ msg); 

  match exprFloat_type with
  | TInt -> print_endline "The expression is an integer."
  | TFloat-> print_endline "The expression is an float."
  | TBool -> print_endline "The expression is an bool."
  | TString -> print_endline "The expression is an string."
  | TError msg -> print_endline ("Type error: " ^ msg) 


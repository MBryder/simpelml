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
  | TArray of typ  (* Assuming you need this for list handling *)

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
  | Ast.Ecst (Ast.Cint _) -> TInt
  | Ast.Ecst (Ast.Cfloat _) -> TFloat
  | Ast.Ecst (Ast.Cbool _) -> TBool
  | Ast.Ecst (Ast.Cstring _) -> TString
  | Ast.Eident {id} -> 
      (try Hashtbl.find env id
       with Not_found -> raise (TypeError ("Unbound variable: " ^ id)))
  | Ast.Ebinop (_, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      if t1 = t2 then t1 else raise (TypeError "Type mismatch in binary operation")
  | Ast.Eunop (_, e) -> type_of_expr env e  (* Simplified: assumes unary op doesn't change type *)
  | Ast.Elist el ->
      TArray (type_of_expr env (List.hd el))  (* Simplified: assumes list is homogeneous *)
  | _ -> raise (TypeError "Unsupported expression type for type checking")


  let rec type_of_stmt env stmt =
    match stmt with
    | Ast.Sif (cond, then_stmt, else_stmt) ->
        let cond_type = type_of_expr env cond in
        (match cond_type with
        | TBool -> 
            type_of_stmt env then_stmt;
            type_of_stmt env else_stmt
        | _ -> raise (TypeError "Condition in if statement must be a boolean"))
  
    | Ast.Sassign (id, expr) ->
        let var_type = Hashtbl.find env id.id in
        let expr_type = type_of_expr env expr in
        if var_type <> expr_type then
          raise (TypeError "Type mismatch in assignment")
  
    | Ast.Sblock stmt_list ->
        List.iter (type_of_stmt env) stmt_list  (* Check all statements in the block *)
  
    | Ast.Sreturn expr ->
        ignore (type_of_expr env expr)  (* Just type check the expression *)
  
    | Ast.Swhile (cond, body) ->
        let cond_type = type_of_expr env cond in
        if cond_type <> TBool then
          raise (TypeError "Condition in while statement must be a boolean")
        else
          type_of_stmt env body
  
    | _ -> raise (TypeError "Unsupported statement type for type checking")
  

(* Example usage *)

(* let _ =
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
  | TError msg -> print_endline ("Type error: " ^ msg)  *)


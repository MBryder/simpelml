(* Type definitions for expressions and types *)
type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Var of string
  | Array of int

type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TError of string
  | TArray of typ

(* Environment to store variable types *)
type env = (string, typ) Hashtbl.t

exception TypeError of string * Ast.location option

(* Type checking function for expressions *)
let rec type_of_expr env = function
  | Ast.Ecst (Ast.Cint _) -> TInt
  | Ast.Ecst (Ast.Cfloat _) -> TFloat
  | Ast.Ecst (Ast.Cbool _) -> TBool
  | Ast.Ecst (Ast.Cstring _) -> TString
  | Ast.Eident { id; loc } -> 
      (* Look up the type of the variable in the environment *)
      (try Hashtbl.find env id
       with Not_found -> raise (TypeError ("Unbound variable: " ^ id, Some loc)))
  | Ast.Ebinop (_, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      if t1 = t2 then t1 else raise (TypeError ("Type mismatch in binary operation", None))
  | Ast.Eunop (_, e) -> type_of_expr env e
  | Ast.Elist el when el <> [] ->
      TArray (type_of_expr env (List.hd el))
  | Ast.Elist _ ->
      raise (TypeError ("Empty lists not allowed", None))
  | _ -> raise (TypeError ("Unsupported expression type for type checking", None))

(* Type checking function for statements *)
let rec type_of_stmt env stmt =
  match stmt with
  | Ast.Sif (cond, then_stmt, else_stmt) ->
      let cond_type = type_of_expr env cond in
      (match cond_type with
      | TBool -> 
          type_of_stmt env then_stmt;
          type_of_stmt env else_stmt
      | _ -> raise (TypeError ("Condition in if statement must be a boolean", None)))
  
  | Ast.Sassign ({ id; loc }, expr) ->
      let expr_type = type_of_expr env expr in
      (try
         let var_type = Hashtbl.find env id in
         if var_type <> expr_type then
           raise (TypeError ("Type mismatch in assignment", Some loc))
       with Not_found ->
         Hashtbl.add env id expr_type)  (* Handle new variable initialization *)
  
  | Ast.Sblock stmt_list ->
      List.iter (type_of_stmt env) stmt_list  (* Check all statements in the block *)
  
      | Ast.Sprint exprs ->
      List.iter (fun expr -> ignore (type_of_expr env expr)) exprs

  | Ast.Sreturn expr ->
      ignore (type_of_expr env expr)  (* Just type check the expression *)
  
  | Ast.Swhile (cond, body) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> TBool then
        raise (TypeError ("Condition in while statement must be a boolean", None))
      else
        type_of_stmt env body
  
  | _ -> raise (TypeError ("Unsupported statement type for type checking", None))

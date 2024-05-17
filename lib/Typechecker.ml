(* Type definitions for expressions and types *)
type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Var of string
  | List of expr list
  | Get of expr * expr
  | Binop of string * expr * expr
  | Unop of string * expr

type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TError of string
  | TList of typ

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
    (try Hashtbl.find env id
     with Not_found -> 
      ignore (TypeError ("Unbound variable: " ^ id, Some loc)); 
      TError "")
  | Ast.Elist elts ->
    (match elts with
      | [] -> raise (TypeError ("Empty lists not allowed", None))
      | hd::tl ->
          let head_type = type_of_expr env hd in
          List.iter (fun elt ->
            let elt_type = type_of_expr env elt in
            if elt_type <> head_type then
              raise (TypeError ("Inconsistent types in list", None))
          ) tl;
          TList head_type)
  | Ast.Eget (list, index) ->
      let list_type = type_of_expr env list in
      let index_type = type_of_expr env index in
      (match list_type, index_type with
       | TList t, TInt -> t
       | TList _, _ -> raise (TypeError ("Index must be an integer", None))
       | _, _ -> raise (TypeError ("Only lists can be indexed", None)))
  | Ast.Ebinop (_, e1, e2) ->
    let t1 = type_of_expr env e1 in
    let t2 = type_of_expr env e2 in
    begin match (t1, t2) with
    | (TInt, TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | (TInt, TFloat) | (TFloat, TInt) -> TFloat  (* Automatically coerce int to float or vice versa *)
    | (TString, TString) -> TString
    | (TBool, TBool) -> TBool
    | _ -> ignore (TypeError ("Type mismatch in binary operation", None)); TError ""
    end
  | Ast.Eunop (_, e) -> type_of_expr env e
  | _ -> ignore (TypeError ("Unsupported expression type for type checking", None)); TError ""

(* Type checking function for statements *)
let rec type_of_stmt env stmt =
  match stmt with
  | Ast.Sif (cond, then_stmt, else_stmt) ->
      let cond_type = type_of_expr env cond in
      (match cond_type with
      | TBool -> 
          type_of_stmt env then_stmt;
          type_of_stmt env else_stmt
      | _ -> ignore (TypeError ("Condition in if statement must be a boolean", None)))

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

  | Ast.Sreturn expr ->
      ignore (type_of_expr env expr)  (* Just type check the expression *)

  | Ast.Swhile (cond, body) ->
      let cond_type = type_of_expr env cond in
      (match cond_type with
      | TBool -> type_of_stmt env body
      | _ -> ignore (TypeError ("Condition in while loop must be a boolean", None)))

  | Ast.Sfor (ident, start_expr, end_expr, body) ->
      let start_type = type_of_expr env start_expr in
      let end_type = type_of_expr env end_expr in
      (match (start_type, end_type) with
       | (TInt, TInt) -> ()
       | _ -> raise (TypeError ("For loop bounds must be integers", None)));
      Hashtbl.add env ident.id TInt;
      type_of_stmt env body;
      Hashtbl.remove env ident.id

  | Ast.Sincr ident ->
      let var_type = Hashtbl.find_opt env ident.id in
      (match var_type with
       | Some TInt | Some TFloat -> ()
       | _ -> raise (TypeError ("Increment operation requires int or float", None)))

  | Ast.Sdecr ident ->
      let var_type = Hashtbl.find_opt env ident.id in
      (match var_type with
       | Some TInt | Some TFloat -> ()
       | _ -> raise (TypeError ("Decrement operation requires int or float", None)))

  | Ast.Spush (list, elem) ->
      let list_type = type_of_expr env list in
      (match list_type with
       | TList elem_type ->
           let elem_type_actual = type_of_expr env elem in
           if elem_type <> elem_type_actual then
             raise (TypeError ("Element type does not match list type", None))
       | _ -> raise (TypeError ("Only lists can be pushed to", None)))

  | Ast.Slist_assign (list, index, new_value) ->
      let list_type = type_of_expr env list in
      let index_type = type_of_expr env index in
      let value_type = type_of_expr env new_value in
      (match (list_type, index_type) with
       | (TList elem_type, TInt) when elem_type = value_type -> ()
       | (TList _, TInt) -> raise (TypeError ("Type mismatch in list assignment", None))
       | (_, _) -> raise (TypeError ("List assignment requires a list and an integer index", None)))

  | Ast.Sprint exprs ->
      List.iter (fun expr -> ignore (type_of_expr env expr)) exprs  (* Check expressions to be printed *)

  | _ -> ignore (TypeError ("Unsupported statement type for type checking", None))

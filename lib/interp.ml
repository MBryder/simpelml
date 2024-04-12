
(* Interpreter for SimpelML.  *)
open Ast
open Format

(* ************************************************************************** *)
(*                                   Utilities                                *)
(* ************************************************************************** *)

(* Exception raised to signal a runtime error *)
exception Error of string
let error s = raise (Error s)

(* Values. *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vfloat of float
  | Vstring of string
  | Vlist of value array


exception Return of value
(* Variables are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)
type ctx = (string, value) Hashtbl.t

(* Print a value on standard output *)
let rec print_value = function
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vfloat f -> printf "%f" f
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"
  | Vnone -> printf "none"

let rec print_values vl = match vl with
  | [] -> printf "@."
  | [v] ->
    print_value v;
    printf "@."
  | v :: vtl ->
    print_value v;
    print_string " ";
    print_values vtl

(* Transpose a matrix implementation. *)
let rec transpose (matrix: value array) : value array =
  match matrix with
  | [||] -> [||]
  | _ ->
    let height = Array.length matrix in
    let width = match matrix.(0) with
      | Vlist l -> Array.length l
      | _ -> error "wrong type: matrix must be a list of lists!"
    in
    let result = Array.init width (fun j -> Vlist (Array.init height (fun i -> 
      match matrix.(i) with
      | Vlist row -> row.(j)
      | _ -> error "wrong type: matrix must be a list of lists!"
    ))) in
    result

(* Mtimes implementation. *)
let mtimes (a: value array) (b: value array) : value array =
  let to_float = function
    | Vint i -> float_of_int i
    | Vfloat f -> f
    | _ -> error "Matrix elements must be integers or floats"
  in
  let a_rows = Array.length a in
  let a_cols = match a.(0) with Vlist a0 -> Array.length a0 | _ -> error "Matrix elements must be arrays" in
  let b_rows = Array.length b in
  let b_cols = match b.(0) with Vlist b0 -> Array.length b0 | _ -> error "Matrix elements must be arrays" in
  if a_cols <> b_rows then
    error "Incompatible matrix dimensions for multiplication"
  else
    let result = Array.init a_rows (fun _ -> Vlist (Array.make b_cols (Vfloat 0.0))) in
    let rec multiply i j k sum =
      if k = a_cols then
        match result.(i) with
        | Vlist res -> res.(j) <- Vfloat sum
        | _ -> error "Matrix elements must be arrays"
      else
        match a.(i), b.(k) with
        | Vlist ai, Vlist bk ->
          let ak = to_float ai.(k) in
          let bk = to_float bk.(j) in
          multiply i j (k + 1) (sum +. ak *. bk)
        | _ -> error "Matrix elements must be arrays"
      in
    let rec multiply_row i j =
      if j = b_cols then ()
      else (
        multiply i j 0 0.0;
        multiply_row i (j + 1)
      )
      in
    let rec multiply_matrix i =
      if i = a_rows then result
      else (
        multiply_row i 0;
        multiply_matrix (i + 1)
      )
      in
    multiply_matrix 0

(* Invert matrix implementation *)
let invert (v: value array) : value array =
  let to_float = function
    | Vint i -> float_of_int i
    | Vfloat f -> f
    | _ -> error "Matrix elements must be integers or floats"
  in
  let matrix = Array.map (function Vlist row -> row | _ -> error "Matrix elements must be arrays") v in
  let n = Array.length matrix in
  let matrix = Array.map (Array.map (fun x -> to_float x)) matrix in
  let id = Array.init n (fun i -> Array.init n (fun j -> if i = j then 1. else 0.)) in

  let rec scale_pivot_row i j pivot =
    if j >= n then ()
    else begin
      matrix.(i).(j) <- matrix.(i).(j) /. pivot;
      id.(i).(j) <- id.(i).(j) /. pivot;
      scale_pivot_row i (j + 1) pivot
    end
    in

  let rec eliminate_row j k i factor =
    if k >= n then ()
    else begin
      matrix.(j).(k) <- matrix.(j).(k) -. factor *. matrix.(i).(k);
      id.(j).(k) <- id.(j).(k) -. factor *. id.(i).(k);
      eliminate_row j (k + 1) i factor
    end
    in

  let rec eliminate_other_rows j i =
    if j >= n then ()
    else if i <> j then begin
      let factor = matrix.(j).(i) in
      eliminate_row j 0 i factor;
      eliminate_other_rows (j + 1) i
    end else
      eliminate_other_rows (j + 1) i
    in

  let rec process_row i =
    if i >= n then ()
    else begin
      let pivot = matrix.(i).(i) in
      if pivot = 0. then error "Matrix is not invertible";
      scale_pivot_row i 0 pivot;
      eliminate_other_rows 0 i;
      process_row (i + 1)
    end
    in

  process_row 0;

  Array.map (fun row -> Vlist (Array.map (fun x -> Vfloat x) row)) id


(* ************************************************************************** *)
(*                          Interpreting expressions                          *)
(* ************************************************************************** *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* Interpreting expressions. *)
let rec interp_expr ctx = function
  | Ecall ({id=f}, el) ->
      if not (Hashtbl.mem functions f) then error (f ^ " is not a function");
      let args, body = Hashtbl.find functions f in
      if List.length args <> List.length el then error ("Inconsistency with expected parameters");
      let ctx' = Hashtbl.create 16 in
      List.iter2 (fun {id=x} e -> Hashtbl.add ctx' x (interp_expr ctx e)) args el;
      begin try stmt ctx' body; Vnone with Return v -> v end  
  | Ecst c -> interp_const c
  | Eunop (op, e1) -> interp_unop ctx op e1
  | Ebinop (op, e1, e2) -> interp_binop ctx op e1 e2
  | Elist el -> Vlist (Array.of_list (List.map (interp_expr ctx) el))
  | Eget (e1, e2) ->
      begin match interp_expr ctx e1 with
      | Vlist l ->
        let i = expr_int ctx e2 in
      (try l.(i) with Invalid_argument _ -> error "index out of bounds")
      | _ -> error "list expected" end
  | Eident {id} -> try Hashtbl.find ctx id with _ -> error "not found"


and expr_int ctx e = match interp_expr ctx e with
  | Vbool false -> 0
  | Vbool true -> 1
  | Vint n -> n
  | _ -> error "This must be an integer"

(* Interpreting constants. *)
and interp_const = function
  | Cbool b ->  Vbool b
  | Cstring s -> Vstring s
  | Cint n -> Vint n
  | Cfloat f -> Vfloat f

(* Interpreting unary operations. *)
and interp_unop ctx op e1 =
  let v1 = interp_expr ctx e1 in
  match op with
  | Uneg ->
    let v1 = interp_expr ctx e1 in
    begin match v1 with
      | Vint n1 -> Vint (-n1)
      | _ -> error "wring unary operand type: argument must be of integer type!"
    end
  | Unot ->
    begin match v1 with
      |  Vbool b1 -> Vbool (not b1)
      | _ -> error "wring unary operand type: argument must be of Boolean type!"
    end
  | Utrans ->
    begin match v1 with
      | Vlist l -> Vlist (transpose l)
      | _ -> error "wrong unary operand type: argument must be a matrix!"
    end
  | Uinv ->
    begin match v1 with
    | Vlist l -> Vlist (invert l)
    | _ -> error "wrong unary operand type: argument must be a matrix!"
    end


(* Interpreting binary operations. *)
and interp_binop ctx op e1 e2 =
  match op with
  | Badd | Bsub | Bmul | Bdiv | Bmod -> interp_binop_arith ctx op e1 e2
  | Bmtimes -> interp_binop_matrix ctx op e1 e2
  | _ (* all other cases *) ->  interp_binop_bool ctx op e1 e2


(* Interpreting binary arithmetic operations return a numerical value. *)
(* We assume that op can only be a binary operation evaluating
   to a integer value, as it is called from the `interp_binop`. *)
and interp_binop_arith ctx op e1 e2 =
  let v1 = interp_expr ctx e1 in
  let v2 = interp_expr ctx e2 in
  match v1, v2 with
  | Vint n1, Vint n2 ->
    begin match op with
        | Badd -> Vint (n1 + n2)
        | Bsub -> Vint (n1 - n2)
        | Bmul -> Vint (n1 * n2)
        | Bmod -> Vint (n1 mod n2)
        | Bdiv -> if n2 = 0 then error "division by zero!" else Vint (n1 / n2)
        | _ -> assert false (* other operations excluded by asssumption. *)
    end
  | Vfloat f1, Vfloat f2 ->
    begin match op with
      | Badd -> Vfloat (f1 +. f2)
      | Bsub -> Vfloat (f1 -. f2)
      | Bmul -> Vfloat (f1 *. f2)
      | Bdiv -> if f2 = 0. then error "division by zero!" else Vfloat (f1 /. f2)
      | _ -> assert false
    end
    | _ -> error "wrong operand type: arguments must be of same numerical type!"

and interp_binop_matrix ctx op e1 e2 = 
  let v1 = interp_expr ctx e1 in
  let v2 = interp_expr ctx e2 in
  match v1, v2 with
  | Vlist a, Vlist b ->
    begin match op with
      | Bmtimes -> Vlist (mtimes a b)
      | _ -> assert false
    end
  | _ -> error "expected a list"

(* Interpreting binary operations returning a Boolean value. *)
(* We assume that op can only be a binary operation evaluating
   to a Boolean value, as it is called from the `interp_binop`. *)
and interp_binop_bool ctx op e1 e2 =
  (* We first treat cases where `op` is a logical operation `Band` or `Bor`
     separately for efficiency. *)
  if op = Band then
    begin match interp_expr ctx e1 with
      | Vbool b1 ->
        if b1 then begin match interp_expr ctx e2 with
          | Vbool b2 -> Vbool b2
          |  _ -> error "unsupported operand types"
        end
        else Vbool false
      | _ -> error "unsupported operand types"
    end
  else if op = Bor then
    match interp_expr ctx e1 with
    | Vbool b1 ->
      if b1 then Vbool true
      else begin match interp_expr ctx e1 with
        | Vbool b2 -> Vbool b2
        | _ ->  error "unsupported operand types"
      end
    | _ ->  error "unsupported operand types"
  else
    (* In all other binary comparision operations, we can evaluate both
       arguments first: *)
    let v1 = interp_expr ctx e1 in
    let v2 = interp_expr ctx e2 in
  match op with
    | Beq  -> Vbool (v1 = v2)
    | Bneq ->  Vbool (not (v1 = v2))
    | Blt  ->  Vbool (v1 < v2)
    | Ble  ->  Vbool (v1 <= v2)
    | Bgt  ->  Vbool (v1 > v2)
    | Bge  ->  Vbool (v1 >= v2)
    | _    -> assert false  (* other operations excluded by asssumption. *)

(* ************************************************************************** *)
(*                          Interpreting statements                           *)
(* ************************************************************************** *)

(* Interpreting a statement *)
and stmt ctx = function 
  | Seval e ->
    ignore (interp_expr ctx e)
  | Sif (e, s1, s2) ->
    begin 
      match interp_expr ctx e with
      | Vbool b1 -> if b1 then stmt ctx s1 else stmt ctx s2
      | _ -> error "wrong type: bool expected"
    end
  | Sassign ({id}, e1) -> 
    Hashtbl.replace ctx id (interp_expr ctx e1)
  | Sblock bl -> 
    block ctx bl
  | Sprint el -> 
    print_values (List.map (fun e -> interp_expr ctx e) el)
  | Swhile (e, s) ->
    begin 
      match interp_expr ctx e with
      | Vbool b -> if b then (stmt ctx s; stmt ctx (Swhile (e, s))) else ()
      | _ -> error "wrong type: bool expected"
    end
  | Sfor ({id; _}, start_expr, end_expr, body_stmt) ->
    let start_val = 
      match interp_expr ctx start_expr with
      | Vint n -> n
      | _ -> error "FOR loop start expression must be integer"
    in
    let end_val = 
      match interp_expr ctx end_expr with
      | Vint n -> n
      | _ -> error "FOR loop end expression must be integer"
    in
    let rec loop i =
      if i <= end_val then begin
        Hashtbl.replace ctx id (Vint i); (* Update the loop variable *)
        stmt ctx body_stmt;              (* Execute the loop body *)
        loop (i + 1)                     (* Recursive call for the next iteration *)
      end
    in
    loop start_val                       (* Start the loop *)
  | Sincr {id} ->
    begin
      match Hashtbl.find_opt ctx id with
      | Some (Vint n) -> Hashtbl.replace ctx id (Vint (n + 1))
      | Some _ -> error "increment operation applied to non-integer"
      | None -> error "variable not found for increment"
    end
  | Sdecr {id} ->
    begin
      match Hashtbl.find_opt ctx id with
      | Some (Vint n) -> Hashtbl.replace ctx id (Vint (n - 1))
      | Some _ -> error "increment operation applied to non-integer"
      | None -> error "variable not found for increment"
    end
  | Slist_assign (arr_expr, idx_expr, new_val_expr) ->
    begin
      match (interp_expr ctx arr_expr, interp_expr ctx idx_expr) with
      | (Vlist arr, Vint i) ->
          if i >= 0 && i < Array.length arr then
            let new_val = interp_expr ctx new_val_expr in
            arr.(i) <- new_val  (* Perform the assignment *)
          else
            error "Array index out of bounds"
      | (_, Vint _) -> error "First expression must be a list"
      | (_, _) -> error "Second expression must be an integer"
    end
  | Sreturn e -> raise (Return (interp_expr ctx e))


and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* ************************************************************************** *)
(*                          Interpreting the program                          *)
(* ************************************************************************** *)

let file (dl, s) =
   List.iter
    (fun (f,args,body) -> Hashtbl.add functions f.id (args, body)) dl;
  stmt (Hashtbl.create 16) s;


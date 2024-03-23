(*
 * Call-by-value reduction   
 *)

exception NotImplemented
exception Stuck

let freshVarCounter = ref 0

(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s. 
 *)
let getFreshVariable s =
  let _ = freshVarCounter := !freshVarCounter + 1 in
  s ^ "__" ^ string_of_int !freshVarCounter

module VarSet = Set.Make (String)

let rec freevar expr =
  match expr with
  | Uml.Var v -> VarSet.singleton v
  | Uml.Lam (bvar, lexpr) -> VarSet.diff (freevar lexpr) (VarSet.singleton bvar)
  | Uml.App (e1, e2) -> VarSet.union (freevar e1) (freevar e2)

let rec swap expr var1 var2 =
  let swap_var v var1 var2 =
    if v = var1 then var2 else if v = var2 then var1 else v
  in
  match expr with
  | Uml.Var v -> Uml.Var (swap_var v var1 var2)
  | Uml.Lam (bvar, lexpr) ->
      Uml.Lam (swap_var bvar var1 var2, swap lexpr var1 var2)
  | Uml.App (e1, e2) -> Uml.App (swap e1 var1 var2, swap e2 var1 var2)

let rec substitute expr var value =
  match expr with
  | Uml.Var v -> if v = var then value else expr
  | Uml.Lam (bvar, lexpr) ->
      if var = bvar then expr
      else
        let value_fv = freevar value in
        if VarSet.find_opt bvar value_fv = None then
          Uml.Lam (bvar, substitute lexpr var value)
        else
          let new_bvar = getFreshVariable bvar in
          Uml.Lam (new_bvar, substitute (swap lexpr bvar new_bvar) var value)
  | Uml.App (e1, e2) ->
      Uml.App (substitute e1 var value, substitute e2 var value)

(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv e =
  match e with
  | Uml.Var _ -> raise Stuck
  | Uml.Lam _ -> raise Stuck
  | Uml.App (e1, e2) -> (
      match e2 with
      | Uml.Lam _ -> (
          match e1 with
          | Uml.Var _ -> raise Stuck
          | Uml.Lam (bvar, lexpr) -> substitute lexpr bvar e2
          | Uml.App _ -> Uml.App (stepv e1, e2))
      | _ -> Uml.App (e1, stepv e2))

let stepOpt stepf e = try Some (stepf e) with Stuck -> None
let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e =
    match stepOpt stepf e with
    | None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in
  Stream.icons e (steps e)

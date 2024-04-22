open Tml

exception NotImplemented
exception Stuck
exception NotConvertible

type stoval = Computed of value | Delayed of exp * env
and stack = Hole_SK | Frame_SK of stack * frame

and state =
  | Anal_ST of stoval Heap.heap * stack * exp * env
  | Return_ST of stoval Heap.heap * stack * value

(* Define your own datatypes *)
and env = ()
and value = ()
and frame = ()

(* Define your own empty environment *)
let emptyEnv = ()

(* Implement the function value2exp : value -> Tml.exp
 * Warning : If you give wrong implementation of this function,
 *           you wiil receive no credit for the entire third part! *)
let value2exp _ = raise NotImplemented

module NameSet = Set.Make (String)

(* Problem 1. 
 * texp2exp : Tml.texp -> Tml.exp *)
let texp2exp te =
  let removeFreeVar name_ls v = List.filter (fun name -> name <> v) name_ls in
  let pushFreeVar name_ls fv = fv :: removeFreeVar name_ls fv in
  let rec collectFreeVar te name_ls bv_set =
    match te with
    | Tvar v ->
        if Option.is_some (NameSet.find_opt v bv_set) then name_ls
        else pushFreeVar name_ls v
    | Tlam (bv, _, lam_te) ->
        removeFreeVar (collectFreeVar lam_te name_ls (NameSet.add bv bv_set)) bv
    | Tapp (abs_te, arg_te) ->
        let abs_name_ctx = collectFreeVar abs_te name_ls bv_set in
        collectFreeVar arg_te abs_name_ctx bv_set
    | Tpair (fst_te, snd_te) ->
        let fst_name_ctx = collectFreeVar fst_te name_ls bv_set in
        collectFreeVar snd_te fst_name_ctx bv_set
    | Tfst te' | Tsnd te' | Tinl (te', _) | Tinr (te', _) ->
        collectFreeVar te' name_ls bv_set
    | Tcase (te', inl_bv, inl_te, inr_bv, inr_te) ->
        let name_ctx' = collectFreeVar te' name_ls bv_set in
        let inl_name_ctx =
          removeFreeVar
            (collectFreeVar inl_te name_ctx' (NameSet.add inl_bv bv_set))
            inl_bv
        in
        removeFreeVar
          (collectFreeVar inr_te inl_name_ctx (NameSet.add inr_bv bv_set))
          inr_bv
    | Tfix (bv, _, fix_te) ->
        removeFreeVar (collectFreeVar fix_te name_ls (NameSet.add bv bv_set)) bv
    | Tifthenelse (cond_te, then_te, else_te) ->
        let cond_name_ctx = collectFreeVar cond_te name_ls bv_set in
        let then_name_ctx = collectFreeVar then_te cond_name_ctx bv_set in
        collectFreeVar else_te then_name_ctx bv_set
    | Tnum _ | Teunit | Ttrue | Tfalse | Tplus | Tminus | Teq -> name_ls
  in
  let name_ls = collectFreeVar te [] NameSet.empty in
  let pushBinder name_ctx bv =
    (bv, 0) :: List.map (fun (v, i) -> (v, i + 1)) name_ctx
  in
  let rec texp2exp' te name_ctx =
    match te with
    | Tvar v ->
        let indices =
          List.map snd (List.filter (fun (v', _) -> v' = v) name_ctx)
        in
        Ind
          (List.fold_left
             (fun x y -> if x < y then x else y)
             (List.hd indices) indices)
    | Tlam (bv, _, lam_te) -> Lam (texp2exp' lam_te (pushBinder name_ctx bv))
    | Tapp (abs_te, arg_te) ->
        App (texp2exp' abs_te name_ctx, texp2exp' arg_te name_ctx)
    | Tpair (fst_te, snd_te) ->
        Pair (texp2exp' fst_te name_ctx, texp2exp' snd_te name_ctx)
    | Tfst te' -> Fst (texp2exp' te' name_ctx)
    | Tsnd te' -> Snd (texp2exp' te' name_ctx)
    | Teunit -> Eunit
    | Tinl (inl_te, _) -> Inl (texp2exp' inl_te name_ctx)
    | Tinr (inr_te, _) -> Inr (texp2exp' inr_te name_ctx)
    | Tcase (case_te, inl_bv, inl_te, inr_bv, inr_te) ->
        Case
          ( texp2exp' case_te name_ctx,
            texp2exp' inl_te (pushBinder name_ctx inl_bv),
            texp2exp' inr_te (pushBinder name_ctx inr_bv) )
    | Tfix (bv, _, fix_te) -> Fix (texp2exp' fix_te (pushBinder name_ctx bv))
    | Ttrue -> True
    | Tfalse -> False
    | Tifthenelse (cond_te, then_te, else_te) ->
        Ifthenelse
          ( texp2exp' cond_te name_ctx,
            texp2exp' then_te name_ctx,
            texp2exp' else_te name_ctx )
    | Tnum n -> Num n
    | Tplus -> Plus
    | Tminus -> Minus
    | Teq -> Eq
  in
  let name_ctx = List.mapi (fun i v -> (v, i)) name_ls in
  texp2exp' te name_ctx

(* Problem 2. 
 * step1 : Tml.exp -> Tml.exp *)
let rec step1 _ = raise Stuck

(* Problem 3. 
 * step2 : state -> state *)
let step2 _ = raise NotImplemented

(* exp2string : Tml.exp -> string *)
let rec exp2string exp =
  match exp with
  | Ind x -> string_of_int x
  | Lam e -> "(lam. " ^ exp2string e ^ ")"
  | App (e1, e2) -> "(" ^ exp2string e1 ^ " " ^ exp2string e2 ^ ")"
  | Pair (e1, e2) -> "(" ^ exp2string e1 ^ "," ^ exp2string e2 ^ ")"
  | Fst e -> "(fst " ^ exp2string e ^ ")"
  | Snd e -> "(snd " ^ exp2string e ^ ")"
  | Eunit -> "()"
  | Inl e -> "(inl " ^ exp2string e ^ ")"
  | Inr e -> "(inr " ^ exp2string e ^ ")"
  | Case (e, e1, e2) ->
      "(case " ^ exp2string e ^ " of " ^ exp2string e1 ^ " | " ^ exp2string e2
      ^ ")"
  | Fix e -> "(fix. " ^ exp2string e ^ ")"
  | Ifthenelse (e, e1, e2) ->
      "(if " ^ exp2string e ^ " then " ^ exp2string e1 ^ " else "
      ^ exp2string e2 ^ ")"
  | True -> "true"
  | False -> "false"
  | Num n -> "<" ^ string_of_int n ^ ">"
  | Plus -> "+"
  | Minus -> "-"
  | Eq -> "="

(* state2string : state -> string 
 * you may modify this function for debugging your code *)
let state2string st =
  match st with
  | Anal_ST (_, _, exp, _) -> "Analysis : ???"
  | Return_ST (_, _, _) -> "Return : ??? "

(* ------------------------------------------------------------- *)
let stepOpt1 e = try Some (step1 e) with Stuck -> None
let stepOpt2 st = try Some (step2 st) with Stuck -> None
let rec multiStep1 e = try multiStep1 (step1 e) with Stuck -> e
let rec multiStep2 st = try multiStep2 (step2 st) with Stuck -> st

let stepStream1 e =
  let rec steps e =
    match stepOpt1 e with
    | None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in
  Stream.icons e (steps e)

let stepStream2 st =
  let rec steps st =
    match stepOpt2 st with
    | None -> Stream.from (fun _ -> None)
    | Some st' -> Stream.icons st' (steps st')
  in
  Stream.icons st (steps st)

open Tml

exception NotImplemented
exception Stuck
exception NotConvertible

module IndexMap = Map.Make (struct
  type t = index

  let compare = compare
end)

type stoval = Computed of value | Delayed of exp * env
and stack = Hole_SK | Frame_SK of stack * frame

and state =
  | Anal_ST of stoval Heap.heap * stack * exp * env
  | Return_ST of stoval Heap.heap * stack * value

(* Define your own datatypes *)
and env = Heap.loc IndexMap.t

and value =
  | True
  | False
  | Num of int
  | Eunit
  | Closure of env * Tml.exp
  | Pair of value * value
  | Inl of value
  | Inr of value
  | Plus
  | Minus
  | Eq

and frame =
  | Heap_ref of Heap.loc
  | App_hole of env * Tml.exp
  | If_hole of env * Tml.exp * Tml.exp
  | Pair_fst_hole of env * Tml.exp
  | Pair_snd_hole of value
  | Fst_hole
  | Snd_hole
  | Inl_hole
  | Inr_hole
  | Case_hole of env * Tml.exp * Tml.exp
  | Plus_hole
  | Minus_hole
  | Eq_hole

(* Define your own empty environment *)
let emptyEnv = IndexMap.empty

(* Implement the function value2exp : value -> Tml.exp
 * Warning : If you give wrong implementation of this function,
 *           you wiil receive no credit for the entire third part! *)
let value2exp v =
  match v with
  | True -> Tml.True
  | False -> Tml.False
  | Num n -> Tml.Num n
  | Eunit -> Tml.Eunit
  | _ -> raise NotConvertible

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
let rec step1 te =
  let rec shift v bv te =
    match te with
    | Ind v' -> if v' >= bv then Ind (v' + v) else Ind v'
    | Lam lam_te -> Lam (shift v (bv + 1) lam_te)
    | App (abs_te, arg_te) -> App (shift v bv abs_te, shift v bv arg_te)
    | Pair (fst_te, snd_te) -> Pair (shift v bv fst_te, shift v bv snd_te)
    | Fst inner_te -> Fst (shift v bv inner_te)
    | Snd inner_te -> Snd (shift v bv inner_te)
    | Inl inner_te -> Inl (shift v bv inner_te)
    | Inr inner_te -> Inr (shift v bv inner_te)
    | Case (case_te, inl_te, inr_te) ->
        Case
          (shift v bv case_te, shift v (bv + 1) inl_te, shift v (bv + 1) inr_te)
    | Fix fix_te -> Fix (shift v (bv + 1) fix_te)
    | Ifthenelse (cond_te, then_te, else_te) ->
        Ifthenelse (shift v bv cond_te, shift v bv then_te, shift v bv else_te)
    | Eunit | True | False | Num _ | Plus | Minus | Eq -> te
  in
  let rec substitute te v te' =
    match te with
    | Ind v' ->
        if v' < v then Ind v' else if v' = v then shift v 0 te' else Ind (v' - 1)
    | Lam lam_te -> Lam (substitute lam_te (v + 1) te')
    | App (abs_te, arg_te) ->
        App (substitute abs_te v te', substitute arg_te v te')
    | Pair (fst_te, snd_te) ->
        Pair (substitute fst_te v te', substitute snd_te v te')
    | Fst inner_te -> Fst (substitute inner_te v te')
    | Snd inner_te -> Snd (substitute inner_te v te')
    | Inl inner_te -> Inl (substitute inner_te v te')
    | Inr inner_te -> Inr (substitute inner_te v te')
    | Case (case_te, inl_te, inr_te) ->
        Case
          ( substitute case_te v te',
            substitute inl_te (v + 1) te',
            substitute inr_te (v + 1) te' )
    | Fix fix_te -> Fix (substitute fix_te (v + 1) te')
    | Ifthenelse (cond_te, then_te, else_te) ->
        Ifthenelse
          ( substitute cond_te v te',
            substitute then_te v te',
            substitute else_te v te' )
    | Eunit | True | False | Num _ | Plus | Minus | Eq -> te
  in
  match te with
  | Ind _ -> raise Stuck
  | Lam _ -> raise Stuck
  | App (abs_te, arg_te) -> (
      try App (step1 abs_te, arg_te)
      with Stuck -> (
        try App (abs_te, step1 arg_te)
        with Stuck -> (
          match (abs_te, arg_te) with
          | Lam lam_te, _ -> substitute lam_te 0 arg_te
          | Plus, Pair (Num lhs, Num rhs) -> Num (lhs + rhs)
          | Minus, Pair (Num lhs, Num rhs) ->
              if lhs < rhs then Num 0 else Num (lhs - rhs)
          | Eq, Pair (Num lhs, Num rhs) -> if lhs = rhs then True else False
          | _ -> raise Stuck)))
  | Pair (fst_te, snd_te) -> (
      try Pair (step1 fst_te, snd_te) with Stuck -> Pair (fst_te, step1 snd_te))
  | Fst te' -> (
      try Fst (step1 te')
      with Stuck -> (
        match te' with Pair (fst_te, _) -> fst_te | _ -> raise Stuck))
  | Snd te' -> (
      try Snd (step1 te')
      with Stuck -> (
        match te' with Pair (_, snd_te) -> snd_te | _ -> raise Stuck))
  | Eunit -> raise Stuck
  | Inl te' -> Inl (step1 te')
  | Inr te' -> Inr (step1 te')
  | Case (case_te, inl_te, inr_te) -> (
      try Case (step1 case_te, inl_te, inr_te)
      with Stuck -> (
        match case_te with
        | Inl inl_val -> substitute inl_te 0 inl_val
        | Inr inr_val -> substitute inr_te 0 inr_val
        | _ -> raise Stuck))
  | Fix fix_te -> substitute fix_te 0 te
  | True -> raise Stuck
  | False -> raise Stuck
  | Ifthenelse (cond_te, then_te, else_te) -> (
      try Ifthenelse (step1 cond_te, then_te, else_te)
      with Stuck -> (
        match cond_te with
        | True -> then_te
        | False -> else_te
        | _ -> raise Stuck))
  | Num _ -> raise Stuck
  | Plus -> raise Stuck
  | Minus -> raise Stuck
  | Eq -> raise Stuck

(* Problem 3. 
 * step2 : state -> state *)
let step2 st =
  let shiftEnv env =
    IndexMap.fold
      (fun idx hloc acc -> IndexMap.add (idx + 1) hloc acc)
      env emptyEnv
  in
  match st with
  | Anal_ST (heap, eval_stack, te, eval_env) -> (
      match te with
      | Ind v -> (
          let heap_index = IndexMap.find v eval_env in
          match Heap.deref heap heap_index with
          | Computed computed_val -> Return_ST (heap, eval_stack, computed_val)
          | Delayed (delayed_te, delayed_env) ->
              Anal_ST
                ( heap,
                  Frame_SK (eval_stack, Heap_ref heap_index),
                  delayed_te,
                  delayed_env ))
      | Lam _ -> Return_ST (heap, eval_stack, Closure (eval_env, te))
      | App (abs_te, arg_te) ->
          Anal_ST
            ( heap,
              Frame_SK (eval_stack, App_hole (eval_env, arg_te)),
              abs_te,
              eval_env )
      | Pair (fst_te, snd_te) ->
          Anal_ST
            ( heap,
              Frame_SK (eval_stack, Pair_fst_hole (eval_env, snd_te)),
              fst_te,
              eval_env )
      | Fst inner_te ->
          Anal_ST (heap, Frame_SK (eval_stack, Fst_hole), inner_te, eval_env)
      | Snd inner_te ->
          Anal_ST (heap, Frame_SK (eval_stack, Snd_hole), inner_te, eval_env)
      | Eunit -> Return_ST (heap, eval_stack, Eunit)
      | Inl inner_te ->
          Anal_ST (heap, Frame_SK (eval_stack, Inl_hole), inner_te, eval_env)
      | Inr inner_te ->
          Anal_ST (heap, Frame_SK (eval_stack, Inr_hole), inner_te, eval_env)
      | Case (case_te, inl_te, inr_te) ->
          Anal_ST
            ( heap,
              Frame_SK (eval_stack, Case_hole (eval_env, inl_te, inr_te)),
              case_te,
              eval_env )
      | Fix fix_te ->
          let new_heap, fix_loc = Heap.allocate heap (Delayed (te, eval_env)) in
          Anal_ST
            ( new_heap,
              eval_stack,
              fix_te,
              IndexMap.add 0 fix_loc (shiftEnv eval_env) )
      | True -> Return_ST (heap, eval_stack, True)
      | False -> Return_ST (heap, eval_stack, False)
      | Ifthenelse (cond_te, then_te, else_te) ->
          Anal_ST
            ( heap,
              Frame_SK (eval_stack, If_hole (eval_env, then_te, else_te)),
              cond_te,
              eval_env )
      | Num n -> Return_ST (heap, eval_stack, Num n)
      | Plus -> Return_ST (heap, eval_stack, Plus)
      | Minus -> Return_ST (heap, eval_stack, Minus)
      | Eq -> Return_ST (heap, eval_stack, Eq))
  | Return_ST (heap, eval_stack, v) -> (
      match eval_stack with
      | Hole_SK -> raise Stuck
      | Frame_SK (below, top) -> (
          match top with
          | Heap_ref loc ->
              let new_heap = Heap.update heap loc (Computed v) in
              Return_ST (new_heap, below, v)
          | App_hole (arg_env, arg_te) -> (
              match v with
              | Closure (closure_env, Lam abs_body_te) ->
                  let new_heap, arg_loc =
                    Heap.allocate heap (Delayed (arg_te, arg_env))
                  in
                  Anal_ST
                    ( new_heap,
                      below,
                      abs_body_te,
                      IndexMap.add 0 arg_loc (shiftEnv closure_env) )
              | Plus ->
                  Anal_ST (heap, Frame_SK (below, Plus_hole), arg_te, arg_env)
              | Minus ->
                  Anal_ST (heap, Frame_SK (below, Minus_hole), arg_te, arg_env)
              | Eq -> Anal_ST (heap, Frame_SK (below, Eq_hole), arg_te, arg_env)
              | _ -> raise Stuck)
          | If_hole (cond_env, then_te, else_te) -> (
              match v with
              | True -> Anal_ST (heap, below, then_te, cond_env)
              | False -> Anal_ST (heap, below, else_te, cond_env)
              | _ -> raise Stuck)
          | Pair_fst_hole (pair_env, snd_te) ->
              Anal_ST (heap, Frame_SK (below, Pair_snd_hole v), snd_te, pair_env)
          | Pair_snd_hole fst_v -> Return_ST (heap, below, Pair (fst_v, v))
          | Fst_hole -> (
              match v with
              | Pair (fst_v, _) -> Return_ST (heap, below, fst_v)
              | _ -> raise Stuck)
          | Snd_hole -> (
              match v with
              | Pair (_, snd_v) -> Return_ST (heap, below, snd_v)
              | _ -> raise Stuck)
          | Inl_hole -> Return_ST (heap, below, Inl v)
          | Inr_hole -> Return_ST (heap, below, Inr v)
          | Case_hole (case_env, inl_te, inr_te) -> (
              match v with
              | Inl v' ->
                  let new_heap, inl_loc = Heap.allocate heap (Computed v') in
                  Anal_ST
                    ( new_heap,
                      below,
                      inl_te,
                      IndexMap.add 0 inl_loc (shiftEnv case_env) )
              | Inr v' ->
                  let new_heap, inr_loc = Heap.allocate heap (Computed v') in
                  Anal_ST
                    ( new_heap,
                      below,
                      inr_te,
                      IndexMap.add 0 inr_loc (shiftEnv case_env) )
              | _ -> raise Stuck)
          | Plus_hole -> (
              match v with
              | Pair (Num lhs, Num rhs) ->
                  Return_ST (heap, below, Num (lhs + rhs))
              | _ -> raise Stuck)
          | Minus_hole -> (
              match v with
              | Pair (Num lhs, Num rhs) ->
                  Return_ST
                    (heap, below, Num (if lhs < rhs then 0 else lhs - rhs))
              | _ -> raise Stuck)
          | Eq_hole -> (
              match v with
              | Pair (Num lhs, Num rhs) ->
                  Return_ST (heap, below, if lhs = rhs then True else False)
              | _ -> raise Stuck)))

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
  let rec value2string v =
    match v with
    | True -> "true"
    | False -> "false"
    | Num n -> "<" ^ string_of_int n ^ ">"
    | Eunit -> "()"
    | Closure (e, te) -> "[" ^ env2string e ^ ", " ^ exp2string te ^ "]"
    | Pair (v1, v2) -> "(" ^ value2string v1 ^ ", " ^ value2string v2 ^ ")"
    | Inl v -> "inl " ^ value2string v
    | Inr v -> "inr " ^ value2string v
    | Plus -> "+"
    | Minus -> "-"
    | Eq -> "="
  and env2string e =
    IndexMap.fold
      (fun idx loc acc ->
        acc ^ ", " ^ string_of_int idx ^ "=>" ^ string_of_int loc)
      e "., "
  and heap2string h =
    let stoval2string s =
      match s with
      | Computed v -> "computed(" ^ value2string v ^ ")"
      | Delayed (te, e) ->
          "delayed(" ^ exp2string te ^ ", " ^ env2string e ^ ")"
    in
    "["
    ^ List.fold_left
        (fun acc (loc, s) ->
          acc ^ ", " ^ string_of_int loc ^ "=>" ^ stoval2string s)
        "., " h
    ^ "]"
  and stack2string st =
    let frame2string f =
      match f with
      | Heap_ref loc -> "[" ^ string_of_int loc ^ "]"
      | App_hole (e, te) -> "_" ^ env2string e ^ " " ^ exp2string te
      | If_hole (e, then_te, else_te) ->
          "if _" ^ env2string e ^ " then " ^ exp2string then_te ^ " else "
          ^ exp2string else_te
      | Pair_fst_hole (e, te) ->
          "(_" ^ env2string e ^ ", " ^ exp2string te ^ ")"
      | Pair_snd_hole v -> "(" ^ value2string v ^ ", _)"
      | Fst_hole -> "fst _"
      | Snd_hole -> "snd _"
      | Inl_hole -> "inl _"
      | Inr_hole -> "inr _"
      | Case_hole (case_env, inl_te, inr_te) ->
          "case _" ^ env2string case_env ^ " of inl " ^ exp2string inl_te
          ^ " | inr " ^ exp2string inr_te
      | Plus_hole -> "plus _"
      | Minus_hole -> "minus _"
      | Eq_hole -> "eq _"
    in
    match st with
    | Hole_SK -> "_"
    | Frame_SK (st', f) -> stack2string st' ^ "; " ^ frame2string f
  in
  match st with
  | Anal_ST (heap, eval_stack, exp, eval_env) ->
      "Analysis : (heap=" ^ heap2string heap ^ ", stack=["
      ^ stack2string eval_stack ^ "], exp=" ^ exp2string exp ^ ", env=["
      ^ env2string eval_env ^ "])"
  | Return_ST (heap, eval_stack, v) ->
      "Return : (heap=" ^ heap2string heap ^ ", stack=["
      ^ stack2string eval_stack ^ "], value=" ^ value2string v ^ ")"

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

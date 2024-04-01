open Tml

exception TypeError

(***************************************************** 
 * replace unit by your own type for typing contexts *
 *****************************************************)
module TypeMap = Map.Make (String)

type context = tp TypeMap.t

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)

let createEmptyContext () = TypeMap.empty

(* val typing : context -> Tml.exp -> Tml.tp *)
let rec typing cxt e =
  match e with
  | Var v -> (
      match TypeMap.find_opt v cxt with Some t -> t | None -> raise TypeError)
  | Lam (bv, bt, el) -> Fun (bt, typing (TypeMap.add bv bt cxt) el)
  | App (e1, e2) -> (
      let t1 = typing cxt e1 in
      let t2 = typing cxt e2 in
      match (t1, t2) with
      | Fun (ta1, tb), ta2 when ta1 = ta2 -> tb
      | _ -> raise TypeError)
  | Pair (e1, e2) -> Prod (typing cxt e1, typing cxt e2)
  | Fst p -> (
      match typing cxt p with Prod (t, _) -> t | _ -> raise TypeError)
  | Snd p -> (
      match typing cxt p with Prod (_, t) -> t | _ -> raise TypeError)
  | Eunit -> Unit
  | Inl (el, tr) -> Sum (typing cxt el, tr)
  | Inr (er, tl) -> Sum (tl, typing cxt er)
  | Case (ec, bvl, el, bvr, er) -> (
      match typing cxt ec with
      | Sum (btl, btr) -> (
          match
            ( typing (TypeMap.add bvl btl cxt) el,
              typing (TypeMap.add bvr btr cxt) er )
          with
          | tl, tr when tl = tr -> tl
          | _ -> raise TypeError)
      | _ -> raise TypeError)
  | Fix (bv, bt, el) ->
      let t = typing (TypeMap.add bv bt cxt) el in
      if bt = t then t else raise TypeError
  | True | False -> Bool
  | Ifthenelse (ec, et, ef) -> (
      match (typing cxt ec, typing cxt et, typing cxt ef) with
      | Bool, tt, tf when tt = tf -> tt
      | _ -> raise TypeError)
  | Num _ -> Int
  | Plus | Minus -> Fun (Prod (Int, Int), Int)
  | Eq -> Fun (Prod (Int, Int), Bool)

let typeOf e = typing (createEmptyContext ()) e
let typeOpt e = try Some (typeOf e) with TypeError -> None

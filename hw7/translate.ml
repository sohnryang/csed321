open Mach
open Mono

exception NotImplemented

module IR_value = struct
  type t = Resolved of rvalue | Placeholder of int

  let to_rvalue value =
    match value with Resolved r -> r | Placeholder i -> REFREG (bp, i)

  exception IR_lvalue_invalid

  let to_lvalue value =
    match value with
    | Resolved rval -> (
        match rval with
        | REG r -> LREG r
        | REFADDR (a, o) -> LREFADDR (a, o)
        | REFREG (r, o) -> LREFREG (r, o)
        | _ -> raise IR_lvalue_invalid)
    | Placeholder i -> LREFREG (bp, i)
end

module IR_inst = struct
  open Mach
  open IR_value

  type t =
    | MOVE of IR_value.t * IR_value.t
    | ADD of IR_value.t * IR_value.t * IR_value.t
    | SUB of IR_value.t * IR_value.t * IR_value.t
    | MUL of IR_value.t * IR_value.t * IR_value.t
    | XOR of IR_value.t * IR_value.t * IR_value.t
    | NOT of IR_value.t * IR_value.t
    | PUSH of IR_value.t
    | POP of IR_value.t
    | MALLOC of IR_value.t * IR_value.t
    | FREE of IR_value.t
    | LABEL of string
    | JUMP of IR_value.t
    | JMPNEQ of IR_value.t * IR_value.t * IR_value.t
    | JMPNEQSTR of IR_value.t * IR_value.t * IR_value.t
    | JMPTRUE of IR_value.t * IR_value.t
    | CALL of IR_value.t
    | RETURN
    | HALT of IR_value.t
    | EXCEPTION

  let to_code inst =
    match inst with
    | MOVE (lval, rval) ->
        Mach.MOVE (IR_value.to_lvalue lval, IR_value.to_rvalue rval)
    | ADD (lval, rval1, rval2) ->
        ADD
          ( IR_value.to_lvalue lval,
            IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2 )
    | SUB (lval, rval1, rval2) ->
        SUB
          ( IR_value.to_lvalue lval,
            IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2 )
    | MUL (lval, rval1, rval2) ->
        MUL
          ( IR_value.to_lvalue lval,
            IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2 )
    | XOR (lval, rval1, rval2) ->
        XOR
          ( IR_value.to_lvalue lval,
            IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2 )
    | NOT (lval, rval) -> NOT (IR_value.to_lvalue lval, IR_value.to_rvalue rval)
    | PUSH rval -> PUSH (IR_value.to_rvalue rval)
    | POP lval -> POP (IR_value.to_lvalue lval)
    | MALLOC (lval, rval) ->
        MALLOC (IR_value.to_lvalue lval, IR_value.to_rvalue rval)
    | FREE rval -> FREE (IR_value.to_rvalue rval)
    | LABEL l -> LABEL l
    | JUMP rval -> JUMP (IR_value.to_rvalue rval)
    | JMPNEQ (rval1, rval2, rval3) ->
        JMPNEQ
          ( IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2,
            IR_value.to_rvalue rval3 )
    | JMPNEQSTR (rval1, rval2, rval3) ->
        JMPNEQSTR
          ( IR_value.to_rvalue rval1,
            IR_value.to_rvalue rval2,
            IR_value.to_rvalue rval3 )
    | JMPTRUE (rval1, rval2) ->
        JMPTRUE (IR_value.to_rvalue rval1, IR_value.to_rvalue rval2)
    | CALL rval -> CALL (IR_value.to_rvalue rval)
    | RETURN -> RETURN
    | HALT rval -> HALT (IR_value.to_rvalue rval)
    | EXCEPTION -> EXCEPTION
end

module NameMap = Map.Make (String)
module NameSet = Set.Make (String)

module IR_trans_env = struct
  type loc = Arg | Context of int | Local of IR_value.t

  type t = {
    next_var_id : int;
    next_label_id : int;
    ctx_mapping : loc NameMap.t;
  }

  let empty =
    { next_var_id = 0; next_label_id = 0; ctx_mapping = NameMap.empty }

  open IR_value

  let create_fresh_var trans_env =
    ( { trans_env with next_var_id = trans_env.next_var_id + 1 },
      Placeholder trans_env.next_var_id )

  let create_fresh_label trans_env =
    ( { trans_env with next_label_id = trans_env.next_label_id + 1 },
      "label_" ^ string_of_int trans_env.next_label_id )
end

module IR_func = struct
  open IR_inst
  open IR_value

  type t = {
    name : label;
    body_insts : IR_inst.t list;
    return_value : IR_value.t;
    local_var_count : int;
  }

  let to_code func =
    let rec repeat l n v = if n = 0 then l else repeat (v :: l) (n - 1) v in
    [ Mach.LABEL func.name ]
    @ repeat [] func.local_var_count (Mach.PUSH (REG zr))
    @ List.map IR_inst.to_code func.body_insts
    @ [ IR_inst.to_code (MOVE (Resolved (REG ax), func.return_value)) ]
    @ repeat [] func.local_var_count (Mach.POP (LREG zr))
    @ [ RETURN ]
end

module IR_block = struct
  exception IR_translation_error

  open IR_value
  open IR_inst
  open IR_trans_env
  open IR_func

  type t = {
    next_env : IR_trans_env.t;
    insts : IR_inst.t list;
    value : IR_value.t;
    dependencies : IR_func.t NameMap.t;
  }

  let rec of_expr trans_env expr =
    match expr with
    | E_INT v ->
        {
          next_env = trans_env;
          insts = [];
          value = IR_value.Resolved (INT v);
          dependencies = NameMap.empty;
        }
    | E_BOOL v ->
        {
          next_env = trans_env;
          insts = [];
          value = IR_value.Resolved (BOOL v);
          dependencies = NameMap.empty;
        }
    | E_UNIT ->
        {
          next_env = trans_env;
          insts = [];
          value = IR_value.Resolved UNIT;
          dependencies = NameMap.empty;
        }
    | E_PLUS | E_MINUS | E_MULT | E_EQ | E_NEQ -> raise IR_translation_error
    | E_VID v -> (
        let name, kind = v in
        match kind with
        | VAR -> (
            let variable_mapping = NameMap.find name trans_env.ctx_mapping in
            match variable_mapping with
            | Arg ->
                {
                  next_env = trans_env;
                  insts = [];
                  value = IR_value.Resolved (REFREG (bp, -3));
                  dependencies = NameMap.empty;
                }
            | Context ctx_index ->
                {
                  next_env = trans_env;
                  insts = [];
                  value = IR_value.Resolved (REFREG (cp, ctx_index));
                  dependencies = NameMap.empty;
                }
            | Local local_var ->
                {
                  next_env = trans_env;
                  insts = [];
                  value = local_var;
                  dependencies = NameMap.empty;
                })
        | CON -> raise NotImplemented
        | CONF -> raise NotImplemented)
    | E_FUN rules -> (
        match rules with
        | [ M_RULE (PATTY (P_VID (arg_name, VAR), _), EXPTY (body_expr, _)) ] ->
            let rec collect_captures current_level_names expr =
              match expr with
              | E_VID (v, VAR) ->
                  if NameSet.find_opt v current_level_names = None then
                    NameSet.singleton v
                  else NameSet.empty
              | E_FUN rules -> (
                  match rules with
                  | [
                   M_RULE
                     (PATTY (P_VID (arg_name', VAR), _), EXPTY (body_expr', _));
                  ] ->
                      NameSet.diff
                        (collect_captures
                           (NameSet.singleton arg_name')
                           body_expr')
                        current_level_names
                  | _ -> raise NotImplemented)
              | E_APP (EXPTY (abs_expr, _), EXPTY (arg_expr, _)) ->
                  NameSet.union
                    (collect_captures current_level_names abs_expr)
                    (collect_captures current_level_names arg_expr)
              | E_PAIR (EXPTY (fst_expr, _), EXPTY (snd_expr, _)) ->
                  NameSet.union
                    (collect_captures current_level_names fst_expr)
                    (collect_captures current_level_names snd_expr)
              | E_LET (decl, EXPTY (inner_expr, _)) -> (
                  match decl with
                  | D_VAL (PATTY (P_VID (var_name, VAR), _), EXPTY (var_expr, _))
                    ->
                      NameSet.union
                        (collect_captures current_level_names var_expr)
                        (collect_captures
                           (NameSet.add var_name current_level_names)
                           inner_expr)
                  | _ -> raise NotImplemented)
              | _ -> NameSet.empty
            in
            let captured_names =
              collect_captures (NameSet.singleton arg_name) body_expr
            in
            let body_ctx_mapping =
              NameMap.add arg_name IR_trans_env.Arg
                (NameSet.fold
                   (fun name mapping ->
                     NameMap.add name
                       (IR_trans_env.Context (NameMap.cardinal mapping))
                       mapping)
                   captured_names NameMap.empty)
            in
            let body_trans_env =
              {
                IR_trans_env.empty with
                ctx_mapping = body_ctx_mapping;
                next_label_id = trans_env.next_label_id;
              }
            in
            let body_translated = of_expr body_trans_env body_expr in
            let next_env, fresh_label =
              IR_trans_env.create_fresh_label
                {
                  trans_env with
                  next_label_id = body_translated.next_env.next_label_id;
                }
            in
            let capturing_copy_insts =
              NameMap.fold
                (fun n l acc ->
                  match (l, NameMap.find_opt n trans_env.ctx_mapping) with
                  | IR_trans_env.Context inner_id, Some outer ->
                      let outer_value =
                        match outer with
                        | Context id -> Resolved (REFREG (cp, id))
                        | Arg -> Resolved (REFREG (bp, -3))
                        | Local v -> v
                      in
                      acc
                      @ [
                          IR_inst.MOVE
                            (Resolved (REFREG (tr, inner_id)), outer_value);
                        ]
                  | _ -> acc)
                body_ctx_mapping []
            in
            {
              next_env;
              insts =
                [
                  IR_inst.MALLOC (Resolved (REG tr), Resolved (INT 2));
                  MOVE
                    ( Resolved (REFREG (tr, 0)),
                      Resolved (ADDR (CADDR fresh_label)) );
                  MALLOC
                    ( Resolved (REFREG (tr, 1)),
                      Resolved (INT (List.length capturing_copy_insts)) );
                  PUSH (Resolved (REG tr));
                  MOVE (Resolved (REG tr), Resolved (REFREG (tr, 1)));
                ]
                @ capturing_copy_insts
                @ [ IR_inst.POP (Resolved (REG tr)) ];
              value = IR_value.Resolved (REG tr);
              dependencies =
                NameMap.add fresh_label
                  {
                    IR_func.name = fresh_label;
                    body_insts = body_translated.insts;
                    return_value = body_translated.value;
                    local_var_count = body_translated.next_env.next_var_id;
                  }
                  body_translated.dependencies;
            }
        | _ -> raise NotImplemented)
    | E_APP (EXPTY (abs_expr, _), EXPTY (arg_expr, _)) -> (
        let arg_block = of_expr trans_env arg_expr in
        match abs_expr with
        | E_PLUS | E_MINUS | E_MULT ->
            let next_env, fresh_var =
              IR_trans_env.create_fresh_var arg_block.next_env
            in
            {
              next_env;
              insts =
                arg_block.insts
                @ [
                    MOVE (Resolved (REG tr), arg_block.value);
                    (match abs_expr with
                    | E_PLUS ->
                        ADD
                          ( fresh_var,
                            Resolved (REFREG (tr, 0)),
                            Resolved (REFREG (tr, 1)) )
                    | E_MINUS ->
                        SUB
                          ( fresh_var,
                            Resolved (REFREG (tr, 0)),
                            Resolved (REFREG (tr, 1)) )
                    | E_MULT ->
                        MUL
                          ( fresh_var,
                            Resolved (REFREG (tr, 0)),
                            Resolved (REFREG (tr, 1)) )
                    | _ -> raise IR_translation_error);
                  ];
              value = fresh_var;
              dependencies = arg_block.dependencies;
            }
        | E_EQ | E_NEQ ->
            let next_env, fresh_label =
              IR_trans_env.create_fresh_label arg_block.next_env
            in
            let next_env, fresh_var = IR_trans_env.create_fresh_var next_env in
            let default_value = abs_expr = E_NEQ in
            {
              next_env;
              insts =
                arg_block.insts
                @ [
                    MOVE (Resolved (REG tr), arg_block.value);
                    MOVE (fresh_var, Resolved (BOOL default_value));
                    JMPNEQ
                      ( Resolved (ADDR (CADDR fresh_label)),
                        Resolved (REFREG (tr, 0)),
                        Resolved (REFREG (tr, 1)) );
                    MOVE (fresh_var, Resolved (BOOL (default_value = false)));
                    LABEL fresh_label;
                  ];
              value = fresh_var;
              dependencies = arg_block.dependencies;
            }
        | _ ->
            let abs_block = of_expr arg_block.next_env abs_expr in
            {
              next_env = abs_block.next_env;
              insts =
                arg_block.insts
                @ [ PUSH (IR_value.Resolved (REG cp)); PUSH arg_block.value ]
                @ abs_block.insts
                @ [
                    MOVE (IR_value.Resolved (REG tr), abs_block.value);
                    MOVE
                      ( IR_value.Resolved (REG cp),
                        IR_value.Resolved (REFREG (tr, 1)) );
                    CALL (IR_value.Resolved (REFREG (tr, 0)));
                    POP (IR_value.Resolved (REG zr));
                    POP (IR_value.Resolved (REG cp));
                  ];
              value = IR_value.Resolved (REG ax);
              dependencies =
                NameMap.union
                  (fun _ f1 f2 ->
                    let () = assert (f1 = f2) in
                    Some f1)
                  abs_block.dependencies arg_block.dependencies;
            })
    | E_PAIR (EXPTY (fst_expr, _), EXPTY (snd_expr, _)) ->
        let fst_block = of_expr trans_env fst_expr in
        let snd_block = of_expr fst_block.next_env snd_expr in
        {
          next_env = snd_block.next_env;
          insts =
            fst_block.insts @ [ PUSH fst_block.value ] @ snd_block.insts
            @ [
                PUSH snd_block.value;
                MALLOC (Resolved (REG tr), Resolved (INT 2));
                POP (Resolved (REFREG (tr, 1)));
                POP (Resolved (REFREG (tr, 0)));
              ];
          value = Resolved (REG tr);
          dependencies =
            NameMap.union
              (fun _ f1 f2 ->
                let () = assert (f1 = f2) in
                Some f1)
              fst_block.dependencies snd_block.dependencies;
        }
    | E_LET (decl, EXPTY (inner_expr, _)) -> (
        match decl with
        | D_VAL (PATTY (P_VID (var_name, VAR), _), EXPTY (var_expr, _)) ->
            let var_expr_block = of_expr trans_env var_expr in
            let next_env, fresh_var =
              IR_trans_env.create_fresh_var var_expr_block.next_env
            in
            let next_env =
              {
                next_env with
                ctx_mapping =
                  NameMap.add var_name (Local fresh_var) next_env.ctx_mapping;
              }
            in
            let inner_block = of_expr next_env inner_expr in
            {
              inner_block with
              insts =
                var_expr_block.insts
                @ [ MOVE (fresh_var, var_expr_block.value) ]
                @ inner_block.insts;
              dependencies =
                NameMap.union
                  (fun _ f1 f2 ->
                    let () = assert (f1 = f2) in
                    Some f1)
                  var_expr_block.dependencies inner_block.dependencies;
            }
        | _ -> raise NotImplemented)
end

(* program2code : Mono.program -> Mach.code *)
let program2code (dlist, et) =
  let non_dtype_defs = List.filter (fun d -> d <> D_DTYPE) dlist in
  let ctx_mapping, _ =
    List.fold_left
      (fun (acc, i) d ->
        match d with
        | D_VAL (PATTY (P_VID (val_name, VAR), _), _) ->
            (NameMap.add val_name (IR_trans_env.Context i) acc, i + 1)
        | _ -> (acc, i))
      (NameMap.empty, 0) non_dtype_defs
  in
  let def_blocks, next_label_id =
    List.fold_left
      (fun (acc, next_label_id) d ->
        let block =
          match d with
          | D_VAL (PATTY (P_VID (val_name, VAR), _), EXPTY (def_expr, _)) ->
              IR_block.of_expr
                { IR_trans_env.ctx_mapping; next_label_id; next_var_id = 0 }
                def_expr
          | _ -> raise NotImplemented
        in
        (acc @ [ block ], block.IR_block.next_env.IR_trans_env.next_label_id))
      ([], 0) non_dtype_defs
  in
  let (EXPTY (expr, _)) = et in
  let expr_block =
    IR_block.of_expr
      { IR_trans_env.ctx_mapping; next_label_id; next_var_id = 0 }
      expr
  in
  let unioned_deps =
    List.fold_left
      (fun acc block ->
        NameMap.union
          (fun _ f1 f2 ->
            let () = assert (f1 = f2) in
            Some f1)
          acc block.IR_block.dependencies)
      NameMap.empty (expr_block :: def_blocks)
  in
  let ctx_mapping_sorted =
    List.sort
      (fun (_, x) (_, y) -> x - y)
      (NameMap.fold
         (fun k v acc ->
           match v with
           | IR_trans_env.Context i -> (k, i) :: acc
           | _ -> raise NotImplemented)
         ctx_mapping [])
  in
  let rec repeat l n v = if n = 0 then l else repeat (v :: l) (n - 1) v in
  [ LABEL Mach.start_label; MALLOC (LREG cp, INT (List.length non_dtype_defs)) ]
  @ List.fold_left
      (fun acc (name, i) ->
        let block = List.nth def_blocks i in
        let locals_count = block.IR_block.next_env.IR_trans_env.next_var_id in
        acc
        @ repeat [] locals_count (PUSH (REG zr))
        @ List.map IR_inst.to_code block.IR_block.insts
        @ [
            IR_inst.to_code
              (IR_inst.MOVE
                 (IR_value.Resolved (REFREG (cp, i)), block.IR_block.value));
          ]
        @ repeat [] locals_count (POP (LREG zr)))
      [] ctx_mapping_sorted
  @ repeat [] expr_block.IR_block.next_env.IR_trans_env.next_var_id
      (PUSH (REG zr))
  @ List.map IR_inst.to_code expr_block.IR_block.insts
  @ repeat [] expr_block.IR_block.next_env.IR_trans_env.next_var_id
      (POP (LREG zr))
  @ [ IR_inst.to_code (IR_inst.HALT expr_block.IR_block.value) ]
  @ NameMap.fold (fun _ func acc -> acc @ IR_func.to_code func) unioned_deps []

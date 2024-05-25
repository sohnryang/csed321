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
    self_name : avid option;
  }

  let empty =
    {
      next_var_id = 0;
      next_label_id = 0;
      ctx_mapping = NameMap.empty;
      self_name = None;
    }

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

module IR_jump_table = struct
  type entry = {
    compare_insts : IR_inst.t list;
    next_label : label;
    variable_mapping : IR_value.t NameMap.t;
  }

  type t = { entries : entry list; next_env : IR_trans_env.t }

  open IR_inst
  open IR_value

  exception IR_pattern_invalid

  let of_patterns trans_env patterns value =
    let rec translate_match trans_env pattern value fail_label =
      let fail_label_value = Resolved (ADDR (CADDR fail_label)) in
      match pattern with
      | P_WILD | P_UNIT -> (trans_env, [], NameMap.empty)
      | P_INT v ->
          ( trans_env,
            [ JMPNEQ (fail_label_value, value, Resolved (INT v)) ],
            NameMap.empty )
      | P_BOOL v ->
          ( trans_env,
            [ JMPNEQ (fail_label_value, value, Resolved (BOOL v)) ],
            NameMap.empty )
      | P_VID (var_name, VAR) ->
          let next_env, fresh_var = IR_trans_env.create_fresh_var trans_env in
          ( next_env,
            [ MOVE (fresh_var, value) ],
            NameMap.singleton var_name fresh_var )
      | P_VID (constructor_name, CON) ->
          ( trans_env,
            [
              JMPNEQSTR
                (fail_label_value, value, Resolved (STR constructor_name));
            ],
            NameMap.empty )
      | P_VIDP ((constructor_name, CONF), PATTY (pattern', _)) ->
          let next_env, insts, mapping =
            translate_match trans_env pattern' (Resolved (REG tr)) fail_label
          in
          ( next_env,
            [
              PUSH (Resolved (REG tr));
              MOVE (Resolved (REG tr), value);
              JMPNEQSTR
                ( fail_label_value,
                  Resolved (REFREG (tr, 0)),
                  Resolved (STR constructor_name) );
              MOVE (Resolved (REG tr), Resolved (REFREG (tr, 1)));
            ]
            @ insts
            @ [ POP (Resolved (REG tr)) ],
            mapping )
      | P_PAIR (PATTY (fst_pattern, _), PATTY (snd_pattern, _)) ->
          let next_env, fst_insts, fst_mapping =
            translate_match trans_env fst_pattern (Resolved (REG tr)) fail_label
          in
          let next_env, snd_insts, snd_mapping =
            translate_match next_env snd_pattern (Resolved (REG tr)) fail_label
          in
          ( next_env,
            [
              PUSH (Resolved (REG tr));
              MOVE (Resolved (REG tr), value);
              PUSH (Resolved (REG tr));
              MOVE (Resolved (REG tr), Resolved (REFREG (tr, 0)));
            ]
            @ fst_insts
            @ [
                POP (Resolved (REG tr));
                MOVE (Resolved (REG tr), Resolved (REFREG (tr, 1)));
              ]
            @ snd_insts
            @ [ POP (Resolved (REG tr)) ],
            NameMap.union
              (fun _ v1 v2 ->
                let () = assert (v1 = v2) in
                Some v1)
              fst_mapping snd_mapping )
      | _ -> raise IR_pattern_invalid
    in
    let entries, next_env =
      List.fold_left
        (fun (acc, trans_env') pattern ->
          let next_env, next_label =
            IR_trans_env.create_fresh_label trans_env'
          in
          let next_env, compare_insts, variable_mapping =
            translate_match next_env pattern value next_label
          in
          (acc @ [ { compare_insts; variable_mapping; next_label } ], next_env))
        ([], trans_env) patterns
    in
    { entries; next_env }
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
        | VAR ->
            let variable_mapping = NameMap.find name trans_env.ctx_mapping in
            let value =
              match variable_mapping with
              | Arg -> IR_value.Resolved (REFREG (bp, -3))
              | Context ctx_index -> IR_value.Resolved (REFREG (cp, ctx_index))
              | Local local_var -> local_var
            in
            {
              next_env = trans_env;
              insts = [];
              value;
              dependencies = NameMap.empty;
            }
        | CON ->
            {
              next_env = trans_env;
              insts = [];
              value = Resolved (STR name);
              dependencies = NameMap.empty;
            }
        | CONF ->
            let constructor_label = "ctor_" ^ name in
            {
              next_env = trans_env;
              insts =
                [
                  IR_inst.MALLOC (Resolved (REG tr), Resolved (INT 2));
                  MOVE
                    ( Resolved (REFREG (tr, 0)),
                      Resolved (ADDR (CADDR constructor_label)) );
                  MALLOC (Resolved (REFREG (tr, 1)), Resolved (INT 0));
                ];
              value = Resolved (REG tr);
              dependencies =
                NameMap.singleton constructor_label
                  {
                    IR_func.name = constructor_label;
                    body_insts =
                      [
                        IR_inst.MALLOC (Resolved (REG tr), Resolved (INT 2));
                        MOVE (Resolved (REFREG (tr, 0)), Resolved (STR name));
                        MOVE
                          (Resolved (REFREG (tr, 1)), Resolved (REFREG (bp, -3)));
                      ];
                    return_value = Resolved (REG tr);
                    local_var_count = 0;
                  };
            })
    | E_FUN rules -> (
        let rec collect_pattern_variables pattern =
          match pattern with
          | P_WILD | P_INT _ | P_BOOL _ | P_UNIT | P_VID (_, CON) ->
              NameSet.empty
          | P_VID (arg_name, VAR) -> NameSet.singleton arg_name
          | P_VIDP ((_, CONF), PATTY (constructor_pattern, _)) ->
              collect_pattern_variables constructor_pattern
          | P_PAIR (PATTY (fst_pattern, _), PATTY (snd_pattern, _)) ->
              NameSet.union
                (collect_pattern_variables fst_pattern)
                (collect_pattern_variables snd_pattern)
          | _ -> raise IR_translation_error
        in
        let rec collect_captures current_level_names expr =
          match expr with
          | E_VID (v, VAR) ->
              if NameSet.find_opt v current_level_names = None then
                NameSet.singleton v
              else NameSet.empty
          | E_FUN rules ->
              NameSet.diff
                (List.fold_left
                   (fun acc rule ->
                     let (M_RULE (PATTY (pattern, _), EXPTY (body_expr, _))) =
                       rule
                     in
                     NameSet.union acc
                       (collect_captures
                          (collect_pattern_variables pattern)
                          body_expr))
                   NameSet.empty rules)
                current_level_names
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
              | D_VAL (PATTY (pattern, _), EXPTY (var_expr, _)) ->
                  NameSet.union
                    (collect_captures current_level_names var_expr)
                    (collect_captures
                       (collect_pattern_variables pattern)
                       inner_expr)
              | D_REC (PATTY (P_VID (var_name, VAR), _), EXPTY (var_expr, _)) ->
                  NameSet.union
                    (collect_captures current_level_names var_expr)
                    (collect_captures
                       (NameSet.add var_name current_level_names)
                       inner_expr)
              | D_DTYPE -> NameSet.empty
              | _ -> raise IR_translation_error)
          | _ -> NameSet.empty
        in
        match rules with
        | [ M_RULE (PATTY (P_VID (arg_name, VAR), _), EXPTY (body_expr, _)) ] ->
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
                        if trans_env.self_name = Some n then
                          Resolved (REFREG (sp, -1))
                        else
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
        | D_REC (PATTY (P_VID (var_name, VAR), _), EXPTY (var_expr, _)) ->
            let next_env, fresh_var = IR_trans_env.create_fresh_var trans_env in
            let var_expr_block =
              of_expr
                {
                  next_env with
                  ctx_mapping =
                    NameMap.add var_name (Local fresh_var) next_env.ctx_mapping;
                  self_name = Some var_name;
                }
                var_expr
            in
            let next_env =
              {
                next_env with
                ctx_mapping =
                  NameMap.add var_name (Local fresh_var) next_env.ctx_mapping;
                self_name = None;
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
        | D_DTYPE -> of_expr trans_env inner_expr
        | _ -> raise NotImplemented)
end

(* program2code : Mono.program -> Mach.code *)
let program2code (dlist, et) =
  let rec collect_bound_var pattern =
    match pattern with
    | P_VID (var_name, VAR) -> NameSet.singleton var_name
    | P_VIDP (_, PATTY (inner_pattern, _)) -> collect_bound_var inner_pattern
    | P_PAIR (PATTY (fst_pattern, _), PATTY (snd_pattern, _)) ->
        NameSet.union
          (collect_bound_var fst_pattern)
          (collect_bound_var snd_pattern)
    | _ -> NameSet.empty
  in
  let bound_vars =
    List.fold_left
      (fun acc d ->
        match d with
        | D_VAL (PATTY (pattern, _), _) ->
            NameSet.union acc (collect_bound_var pattern)
        | D_REC (PATTY (P_VID (val_name, VAR), _), _) ->
            NameSet.add val_name acc
        | _ -> acc)
      NameSet.empty dlist
  in
  let ctx_mapping =
    NameSet.fold
      (fun v acc ->
        NameMap.add v (IR_trans_env.Context (NameMap.cardinal acc)) acc)
      bound_vars NameMap.empty
  in
  let blocks, jump_tables, next_label_id =
    List.fold_left
      (fun (acc_blocks, acc_jump_tables, next_label_id) d ->
        match d with
        | D_VAL (PATTY (pattern, _), EXPTY (expr, _)) ->
            let block =
              IR_block.of_expr
                {
                  IR_trans_env.empty with
                  IR_trans_env.ctx_mapping;
                  next_label_id;
                }
                expr
            in
            let jump_table =
              IR_jump_table.of_patterns block.IR_block.next_env [ pattern ]
                block.IR_block.value
            in
            ( acc_blocks @ [ block ],
              acc_jump_tables @ [ jump_table ],
              jump_table.IR_jump_table.next_env.IR_trans_env.next_label_id )
        | D_REC (PATTY (P_VID (var_name, var_is), _), EXPTY (expr, _)) ->
            let block =
              IR_block.of_expr
                {
                  IR_trans_env.empty with
                  IR_trans_env.ctx_mapping;
                  next_label_id;
                  self_name = Some var_name;
                }
                expr
            in
            let jump_table =
              IR_jump_table.of_patterns
                { block.IR_block.next_env with IR_trans_env.self_name = None }
                [ P_VID (var_name, var_is) ]
                block.IR_block.value
            in
            ( acc_blocks @ [ block ],
              acc_jump_tables @ [ jump_table ],
              jump_table.IR_jump_table.next_env.IR_trans_env.next_label_id )
        | _ -> (acc_blocks, acc_jump_tables, next_label_id))
      ([], [], 0) dlist
  in
  let unioned_deps =
    List.fold_left
      (fun acc b ->
        NameMap.union
          (fun _ f1 f2 ->
            let () = assert (f1 = f2) in
            Some f1)
          acc b.IR_block.dependencies)
      NameMap.empty blocks
  in
  let rec repeat l n v = if n = 0 then l else repeat (v :: l) (n - 1) v in
  let def_insts, next_label_id =
    List.fold_left2
      (fun (acc, next_label_id) b j ->
        let local_count = j.IR_jump_table.next_env.IR_trans_env.next_var_id in
        let next_env, finish_label =
          IR_trans_env.create_fresh_label
            { IR_trans_env.empty with IR_trans_env.next_label_id }
        in
        let entry = List.hd j.IR_jump_table.entries in
        ( acc
          @ repeat [] local_count (IR_inst.PUSH (Resolved (REG zr)))
          @ b.IR_block.insts @ entry.IR_jump_table.compare_insts
          @ NameMap.fold
              (fun name v acc ->
                match NameMap.find_opt name ctx_mapping with
                | Some (Context i) ->
                    acc
                    @ [ IR_inst.MOVE (IR_value.Resolved (REFREG (cp, i)), v) ]
                | _ -> acc)
              entry.IR_jump_table.variable_mapping []
          @ repeat [] local_count (IR_inst.POP (Resolved (REG zr)))
          @ [
              JUMP (Resolved (ADDR (CADDR finish_label)));
              LABEL entry.next_label;
              EXCEPTION;
              LABEL finish_label;
            ],
          next_env.IR_trans_env.next_label_id ))
      ([], next_label_id) blocks jump_tables
  in
  let (EXPTY (expr, _)) = et in
  let expr_block =
    IR_block.of_expr { IR_trans_env.empty with ctx_mapping; next_label_id } expr
  in
  let expr_insts =
    repeat [] expr_block.IR_block.next_env.next_var_id
      (IR_inst.PUSH (Resolved (REG zr)))
    @ expr_block.insts
    @ repeat [] expr_block.IR_block.next_env.next_var_id
        (IR_inst.POP (Resolved (REG zr)))
    @ [ HALT expr_block.value ]
  in
  let unioned_deps =
    NameMap.union
      (fun _ f1 f2 ->
        let () = assert (f1 = f2) in
        Some f1)
      unioned_deps expr_block.dependencies
  in
  List.map IR_inst.to_code
    ([
       IR_inst.LABEL Mach.start_label;
       IR_inst.MALLOC
         (Resolved (REG cp), Resolved (INT (NameSet.cardinal bound_vars)));
     ]
    @ def_insts @ expr_insts)
  @ NameMap.fold (fun _ func acc -> acc @ IR_func.to_code func) unioned_deps []

open Fjava

exception NotImplemented
exception TypeError
exception Stuck

module NameMap' = Map.Make (String)

module NameMap = struct
  include NameMap'

  let find_with_type_error name m =
    try NameMap'.find name m with Not_found -> raise TypeError

  let find_with_stuck name m =
    try NameMap'.find name m with Not_found -> raise Stuck
end

module Param = struct
  type t = { name : string; t : typ }
end

module Constructor = struct
  type t = {
    name : typ;
    params : Param.t list;
    super_args : string list;
    assignments : (string * string) list;
  }
end

module Method = struct
  type t = {
    name : string;
    return_type : typ;
    params : Param.t list;
    return_exp : exp;
  }
end

module Class = struct
  type t = {
    name : typ;
    super_name : typ;
    fields : typ NameMap.t;
    constructor : Constructor.t;
    methods : Method.t NameMap.t;
  }
end

module ClassTable = struct
  type t = Class.t NameMap.t

  let rec is_subtype table lhs rhs =
    if lhs = rhs then true
    else if lhs = "Object" then false
    else
      let lhs_decl = NameMap.find_with_type_error lhs table in
      is_subtype table lhs_decl.Class.super_name rhs

  let rec field_owner table t field =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find_with_type_error t table in
      match NameMap.find_opt field decl.Class.fields with
      | Some _ -> decl
      | None -> field_owner table decl.super_name field

  let rec method_owner table t method_name =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find_with_type_error t table in
      match NameMap.find_opt method_name decl.Class.methods with
      | Some _ -> decl
      | None -> method_owner table decl.super_name method_name

  let mapped_constructor_arg table t field =
    let owner = field_owner table t field in
    let rec collect_chain acc current_t =
      let current_decl = NameMap.find_with_type_error t table in
      if current_t = owner.name then acc
      else collect_chain (current_decl :: acc) current_decl.super_name
    in
    let chain = collect_chain [] t in
    let owner_mapping =
      List.fold_left
        (fun acc (lhs, rhs) -> NameMap.add lhs rhs acc)
        NameMap.empty owner.constructor.assignments
    in
    let mapping =
      List.fold_left
        (fun acc decl ->
          let super_decl =
            NameMap.find_with_type_error decl.Class.super_name table
          in
          let super_mapping =
            List.fold_left
              (fun acc (p, a) -> NameMap.add p.Param.name a acc)
              NameMap.empty
              (List.combine super_decl.Class.constructor.params
                 decl.constructor.super_args)
          in
          NameMap.fold
            (fun field super_p acc ->
              NameMap.add field
                (NameMap.find_with_type_error super_p super_mapping)
                acc)
            acc NameMap.empty)
        owner_mapping chain
    in
    NameMap.find_with_type_error field mapping
end

type typing_context = typ NameMap.t

let convert_classes class_decls =
  let convert_params params =
    List.map (fun (t, name) -> { Param.name; t }) params
  in
  let convert_constructor constructor_decl =
    let name, params, super_args, assignments = constructor_decl in
    {
      Constructor.name;
      params = convert_params params;
      super_args;
      assignments;
    }
  in
  let convert_method method_decl =
    let return_type, name, params, return_exp = method_decl in
    { Method.name; return_type; params = convert_params params; return_exp }
  in
  let convert_methods method_decls =
    List.fold_left
      (fun acc method_decl ->
        let converted = convert_method method_decl in
        NameMap.add converted.name converted acc)
      NameMap.empty method_decls
  in
  let convert_class class_decl =
    let name, super_name, fields, constructor, methods = class_decl in
    {
      Class.name;
      super_name;
      fields =
        List.fold_left
          (fun acc (t, n) -> NameMap.add n t acc)
          NameMap.empty fields;
      constructor = convert_constructor constructor;
      methods = convert_methods methods;
    }
  in
  List.fold_left
    (fun acc class_decl ->
      let converted = convert_class class_decl in
      NameMap.add converted.name converted acc)
    (NameMap.add "Object"
       {
         Class.name = "Object";
         super_name = "Object";
         fields = NameMap.empty;
         constructor =
           { name = "Object"; params = []; super_args = []; assignments = [] };
         methods = NameMap.empty;
       }
       NameMap.empty)
    class_decls

let typeOf p =
  let class_decls, expr = p in
  let table = convert_classes class_decls in
  let rec resolve_type type_ctx expr =
    match expr with
    | Var v -> NameMap.find_with_type_error v type_ctx
    | Field (expr, field_name) ->
        let expr_t = resolve_type type_ctx expr in
        let field_owner = ClassTable.field_owner table expr_t field_name in
        NameMap.find_with_type_error field_name field_owner.fields
    | Method (expr, method_name, args) ->
        let expr_t = resolve_type type_ctx expr in
        let method_owner = ClassTable.method_owner table expr_t method_name in
        let method_decl =
          NameMap.find_with_type_error method_name method_owner.methods
        in
        if List.length method_decl.params <> List.length args then
          raise TypeError
        else if
          List.for_all
            (fun (param, arg) ->
              let arg_t = resolve_type type_ctx arg in
              ClassTable.is_subtype table arg_t param.Param.t)
            (List.combine method_decl.params args)
        then method_decl.return_type
        else raise TypeError
    | New (new_t, args) ->
        let new_t_decl = NameMap.find_with_type_error new_t table in
        let params = new_t_decl.constructor.params in
        if List.length params <> List.length args then raise TypeError
        else if
          List.for_all
            (fun (param, arg) ->
              let arg_t = resolve_type type_ctx arg in
              ClassTable.is_subtype table arg_t param.Param.t)
            (List.combine params args)
        then new_t
        else raise TypeError
    | Cast (new_t, casted_expr) ->
        let casted_t = resolve_type type_ctx casted_expr in
        let () =
          if
            ClassTable.is_subtype table new_t casted_t = false
            && ClassTable.is_subtype table casted_t new_t = false
          then print_endline "Stupid Warning"
          else ()
        in
        new_t
  and check_method base_class method_decl =
    let type_ctx =
      List.fold_left
        (fun acc param -> NameMap.add param.Param.name param.t acc)
        (NameMap.add "this" base_class NameMap.empty)
        method_decl.Method.params
    in
    ClassTable.is_subtype table
      (resolve_type type_ctx method_decl.return_exp)
      method_decl.return_type
  in
  let check_class name =
    let decl = NameMap.find_with_type_error name table in
    let constructor = decl.Class.constructor in
    let super_decl = NameMap.find_with_type_error decl.super_name table in
    if decl.name <> constructor.name then false
    else if
      List.length super_decl.constructor.params
      <> List.length constructor.super_args
    then false
    else if
      List.exists
        (fun (super_param, arg) ->
          let arg_t =
            (try List.find (fun p -> arg = p.Param.name) constructor.params
             with Not_found -> raise TypeError)
              .t
          in
          ClassTable.is_subtype table arg_t super_param.Param.t = false)
        (List.combine super_decl.constructor.params constructor.super_args)
    then false
    else if
      List.exists
        (fun (lhs, rhs) ->
          let lhs_t = NameMap.find_with_type_error lhs decl.fields in
          let rhs_t =
            (try List.find (fun p -> rhs = p.Param.name) constructor.params
             with Not_found -> raise TypeError)
              .t
          in
          ClassTable.is_subtype table rhs_t lhs_t = false)
        constructor.assignments
    then false
    else if
      NameMap.exists
        (fun method_name super_method_decl ->
          let method_decl =
            NameMap.find_with_type_error method_name decl.methods
          in
          List.map (fun p -> p.Param.t) super_method_decl.Method.params
          <> List.map (fun p -> p.Param.t) method_decl.params
          || super_method_decl.return_type <> method_decl.return_type)
        (NameMap.filter
           (fun method_name _ ->
             NameMap.find_opt method_name super_decl.methods <> None)
           decl.methods)
    then false
    else NameMap.for_all (fun _ m -> check_method decl.name m) decl.methods
  in
  if NameMap.for_all (fun name _ -> check_class name) table then
    resolve_type NameMap.empty expr
  else raise TypeError

let step p =
  let class_decls, expr = p in
  let table = convert_classes class_decls in
  let rec step' expr =
    let rec reduce_args args =
      match args with
      | arg :: t -> ( try step' arg :: t with Stuck -> arg :: reduce_args t)
      | [] -> raise Stuck
    in
    match expr with
    | Var _ -> raise Stuck
    | Field (base_expr, field_name) -> (
        try Field (step' base_expr, field_name)
        with Stuck -> (
          match base_expr with
          | New (new_t, args) ->
              let mapped_arg =
                ClassTable.mapped_constructor_arg table new_t field_name
              in
              let new_t_decl = NameMap.find_with_stuck new_t table in
              snd
                (List.find
                   (fun (p, _) -> p.Param.name = mapped_arg)
                   (List.combine new_t_decl.constructor.params args))
          | _ -> raise Stuck))
    | Method (base_expr, method_name, args) -> (
        try Method (step' base_expr, method_name, args)
        with Stuck -> (
          match base_expr with
          | New (new_t, new_args) -> (
              try Method (base_expr, method_name, reduce_args args)
              with Stuck ->
                let owner = ClassTable.method_owner table new_t method_name in
                let method_decl =
                  NameMap.find_with_stuck method_name owner.methods
                in
                let return_expr = method_decl.return_exp in
                let rec substitute this_expr args expr =
                  match expr with
                  | Var v ->
                      if v = "this" then this_expr
                      else NameMap.find_with_stuck v args
                  | Field (base_expr, field_name) ->
                      Field (substitute this_expr args base_expr, field_name)
                  | Method (base_expr, method_name, call_args) ->
                      Method
                        ( substitute this_expr args base_expr,
                          method_name,
                          List.map (substitute this_expr args) call_args )
                  | New (new_t, new_args) ->
                      New (new_t, List.map (substitute this_expr args) new_args)
                  | Cast (new_t, casted_expr) ->
                      Cast (new_t, substitute this_expr args casted_expr)
                in
                substitute
                  (New (new_t, new_args))
                  (List.fold_left
                     (fun acc (p, a) -> NameMap.add p.Param.name a acc)
                     NameMap.empty
                     (List.combine method_decl.params args))
                  return_expr)
          | _ -> raise Stuck))
    | New (new_t, args) -> New (new_t, reduce_args args)
    | Cast (new_t, casted_expr) -> (
        try Cast (new_t, step' casted_expr)
        with Stuck -> (
          match casted_expr with
          | New (inner_t, _) when ClassTable.is_subtype table inner_t new_t ->
              casted_expr
          | _ -> raise Stuck))
  in
  (class_decls, step' expr)

let typeOpt p = try Some (typeOf p) with TypeError -> None
let stepOpt p = try Some (step p) with Stuck -> None
let rec multiStep p = try multiStep (step p) with Stuck -> p

let rec stepStream e =
  let rec steps e =
    match stepOpt e with
    | None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in
  Stream.icons e (steps e)

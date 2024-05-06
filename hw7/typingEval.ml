open Fjava

exception NotImplemented
exception TypeError
exception Stuck

module NameSet = Set.Make (String)
module NameMap' = Map.Make (String)

module NameMap = struct
  include NameMap'

  let find_or name m exn =
    try NameMap'.find name m with Not_found -> raise exn
end

module Param = struct
  type t = { name : string; t : typ }
end

module ParamList = struct
  type t = Param.t list

  let find_param name l = List.find (fun p -> p.Param.name = name) l
  let find_param_opt name l = List.find_opt (fun p -> p.Param.name = name) l

  let find_param_or name l exn =
    try find_param name l with Not_found -> raise exn
end

module Constructor = struct
  type t = {
    name : typ;
    params : ParamList.t;
    super_args : string list;
    assignments : (string * string) list;
  }
end

module Method = struct
  type t = {
    name : string;
    return_type : typ;
    params : ParamList.t;
    return_exp : exp;
  }
end

module Class = struct
  type t = {
    name : typ;
    super_name : typ;
    fields : ParamList.t;
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
      let lhs_decl = NameMap.find_or lhs table TypeError in
      is_subtype table lhs_decl.Class.super_name rhs

  let rec field_owner table t field =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find_or t table TypeError in
      match ParamList.find_param_opt field decl.Class.fields with
      | Some _ -> decl
      | None -> field_owner table decl.super_name field

  let rec method_owner table t method_name =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find_or t table TypeError in
      match NameMap.find_opt method_name decl.Class.methods with
      | Some _ -> decl
      | None -> method_owner table decl.super_name method_name
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
      fields = List.map (fun (t, name) -> { Param.name; t }) fields;
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
         fields = [];
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
    | Var v -> NameMap.find_or v type_ctx TypeError
    | Field (expr, field_name) ->
        let expr_t = resolve_type type_ctx expr in
        let field_owner = ClassTable.field_owner table expr_t field_name in
        (ParamList.find_param_or field_name field_owner.fields TypeError).t
    | Method (expr, method_name, args) ->
        let expr_t = resolve_type type_ctx expr in
        let method_owner = ClassTable.method_owner table expr_t method_name in
        let method_decl =
          NameMap.find_or method_name method_owner.methods TypeError
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
        let new_t_decl = NameMap.find_or new_t table TypeError in
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
    let decl = NameMap.find_or name table TypeError in
    let constructor = decl.Class.constructor in
    let super_decl = NameMap.find_or decl.super_name table TypeError in
    if decl.name <> constructor.name then false
    else if
      List.exists
        (fun (f, p) -> f.Param.name <> p.Param.name)
        (List.combine decl.fields constructor.params)
    then false
    else if
      NameSet.cardinal
        (NameSet.of_list (List.map (fun p -> p.Param.name) constructor.params))
      <> List.length constructor.params
    then false
    else if List.exists (fun (lhs, rhs) -> lhs <> rhs) constructor.assignments
    then false
    else if super_decl.constructor.params @ decl.fields <> constructor.params
    then false
    else if
      NameMap.exists
        (fun method_name method_decl ->
          let super_method_decl =
            NameMap.find_or method_name super_decl.methods TypeError
          in
          List.map (fun p -> p.Param.t) method_decl.Method.params
          <> List.map (fun p -> p.Param.t) super_method_decl.params
          || method_decl.return_type <> super_method_decl.return_type)
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
              let new_t_decl = NameMap.find_or new_t table Stuck in
              snd
                (List.find
                   (fun (p, a) -> p.Param.name = field_name)
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
                  NameMap.find_or method_name owner.methods Stuck
                in
                let return_expr = method_decl.return_exp in
                let rec substitute this_expr args expr =
                  match expr with
                  | Var v ->
                      if v = "this" then this_expr
                      else NameMap.find_or v args Stuck
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

open Fjava

exception NotImplemented
exception TypeError
exception Stuck

module NameMap' = Map.Make (String)

module NameMap = struct
  include NameMap'

  let find name m = try NameMap'.find name m with Not_found -> raise TypeError
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

module ReducedExpr = struct
  type t = { casted_t : typ; new_t : typ; new_args : exp list }

  let to_exp rexp =
    if rexp.casted_t = rexp.new_t then New (rexp.new_t, rexp.new_args)
    else Cast (rexp.casted_t, New (rexp.new_t, rexp.new_args))
end

type class_table = Class.t NameMap.t
type typing_context = typ NameMap.t
type eval_context = ReducedExpr.t NameMap.t

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
  let rec is_subtype lhs rhs =
    if lhs = rhs then true
    else if lhs = "Object" then false
    else
      let lhs_decl = NameMap.find lhs table in
      is_subtype lhs_decl.Class.super_name rhs
  in
  let rec lookup_field_owner t field =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find t table in
      match NameMap.find_opt field decl.fields with
      | Some _ -> decl
      | None -> lookup_field_owner decl.super_name field
  in
  let rec lookup_method_owner t method_name =
    if t = "Object" then raise TypeError
    else
      let decl = NameMap.find t table in
      match NameMap.find_opt method_name decl.methods with
      | Some _ -> decl
      | None -> lookup_method_owner decl.super_name method_name
  in
  let rec resolve_type type_ctx expr =
    match expr with
    | Var v -> NameMap.find v type_ctx
    | Field (expr, field_name) ->
        let expr_t = resolve_type type_ctx expr in
        let field_owner = lookup_field_owner expr_t field_name in
        NameMap.find field_name field_owner.fields
    | Method (expr, method_name, args) ->
        let expr_t = resolve_type type_ctx expr in
        let method_owner = lookup_method_owner expr_t method_name in
        let method_decl = NameMap.find method_name method_owner.methods in
        if List.length method_decl.params <> List.length args then
          raise TypeError
        else if
          List.for_all
            (fun (param, arg) ->
              let arg_t = resolve_type type_ctx arg in
              is_subtype arg_t param.Param.t)
            (List.combine method_decl.params args)
        then method_decl.return_type
        else raise TypeError
    | New (new_t, args) ->
        let new_t_decl = NameMap.find new_t table in
        let params = new_t_decl.constructor.params in
        if List.length params <> List.length args then raise TypeError
        else if
          List.for_all
            (fun (param, arg) ->
              let arg_t = resolve_type type_ctx arg in
              is_subtype arg_t param.Param.t)
            (List.combine params args)
        then new_t
        else raise TypeError
    | Cast (new_t, casted_expr) ->
        let casted_t = resolve_type type_ctx casted_expr in
        let () =
          if
            is_subtype new_t casted_t = false
            && is_subtype casted_t new_t = false
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
    is_subtype
      (resolve_type type_ctx method_decl.return_exp)
      method_decl.return_type
  in
  let check_class name =
    let decl = NameMap.find name table in
    let constructor = decl.Class.constructor in
    let super_decl = NameMap.find decl.super_name table in
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
          is_subtype arg_t super_param.Param.t = false)
        (List.combine super_decl.constructor.params constructor.super_args)
    then false
    else if
      List.exists
        (fun (lhs, rhs) ->
          let lhs_t = NameMap.find lhs decl.fields in
          let rhs_t =
            (try List.find (fun p -> rhs = p.Param.name) constructor.params
             with Not_found -> raise TypeError)
              .t
          in
          is_subtype rhs_t lhs_t = false)
        constructor.assignments
    then false
    else if
      NameMap.exists
        (fun method_name super_method_decl ->
          let method_decl = NameMap.find method_name decl.methods in
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

let step p = raise Stuck
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

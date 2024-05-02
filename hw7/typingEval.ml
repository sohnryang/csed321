open Fjava

exception NotImplemented
exception TypeError
exception Stuck

let typeOf p = raise NotImplemented

let step p = raise NotImplemented

let typeOpt p = try Some (typeOf p) with TypeError -> None
let stepOpt p = try Some (step p) with Stuck -> None
let rec multiStep p = try multiStep (step p) with Stuck -> p

let rec stepStream e =
  let rec steps e =
    match stepOpt e with
        None -> Stream.from (fun _ -> None)
      | Some e' -> Stream.icons e' (steps e')
  in Stream.icons e (steps e)

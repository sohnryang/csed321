open Common

exception NotImplemented
exception IllegalFormat

module Integer : SCALAR with type t = int = struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1
  let ( ++ ) x y = x + y
  let ( ** ) x y = x * y
  let ( == ) x y = x = y
end

(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool = struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true
  let ( ++ ) x y = x || y
  let ( ** ) x y = x && y
  let ( == ) x y = x = y
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t = struct
  type elem = Scal.t
  type t = elem list

  exception VectorIllegal

  let create l = match l with [] -> raise VectorIllegal | _ -> l
  let to_list v = v
  let dim v = List.length v

  let nth v n =
    match List.nth_opt v n with Some x -> x | None -> raise VectorIllegal

  let ( ++ ) x y =
    if dim x = dim y then List.map2 Scal.( ++ ) x y else raise VectorIllegal

  let ( == ) x y =
    if dim x = dim y then List.for_all2 Scal.( == ) x y else raise VectorIllegal

  let innerp x y =
    if dim x = dim y then
      List.fold_left Scal.( ++ ) Scal.zero (List.map2 Scal.( ** ) x y)
    else raise VectorIllegal
end

(* Problem 1-3 *)
(* Matrices *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t = struct
  type elem = Scal.t
  type t = elem list list

  exception MatrixIllegal

  let create l =
    if
      List.length l <> 0
      && List.for_all (fun r -> List.length r = List.length l) l
    then l
    else raise MatrixIllegal

  let init n f =
    let rec init' n f l =
      if n > 0 then init' (n - 1) f (f (n - 1) :: l) else l
    in
    init' n f []

  let identity n =
    if n > 0 then
      init n (fun r ->
          init n (fun i ->
              match i = r with true -> Scal.one | false -> Scal.zero))
    else raise MatrixIllegal

  let dim m = List.length m
  let transpose m = init (dim m) (fun c -> List.map (fun r -> List.nth r c) m)
  let to_list m = m

  let get m r c =
    let row =
      match List.nth_opt m r with Some l -> l | None -> raise MatrixIllegal
    in
    match List.nth_opt row c with Some e -> e | None -> raise MatrixIllegal

  let ( ++ ) x y =
    let open VectorFn (Scal) in
    try List.map2 (fun v w -> to_list (create v ++ create w)) x y
    with VectorIllegal -> raise MatrixIllegal

  let ( ** ) x y =
    let open VectorFn (Scal) in
    try
      List.map
        (fun r ->
          List.map (fun c -> innerp (create r) (create c)) (transpose y))
        x
    with VectorIllegal -> raise MatrixIllegal

  let ( == ) x y =
    let open VectorFn (Scal) in
    try List.for_all2 (fun v w -> create v == create w) x y
    with VectorIllegal -> raise MatrixIllegal
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) : sig
  val closure : Mat.t -> Mat.t
end = struct
  let closure x =
    let rec closure' x c =
      let next = Mat.(identity (dim x) ++ (x ** c)) in
      if Mat.(c == next) then c else closure' x next
    in
    closure' x (Mat.identity (Mat.dim x))
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach g =
  try g |> BoolMat.create |> BoolMatClosure.closure |> BoolMat.to_list
  with BoolMat.MatrixIllegal -> raise IllegalFormat

let al =
  [
    [ true; false; false; false; false; false ];
    [ false; true; true; true; false; false ];
    [ false; true; true; false; true; false ];
    [ false; true; false; true; true; true ];
    [ false; false; true; true; true; false ];
    [ false; false; false; true; false; true ];
  ]

let solution_al' =
  [
    [ true; false; false; false; false; false ];
    [ false; true; true; true; true; true ];
    [ false; true; true; true; true; true ];
    [ false; true; true; true; true; true ];
    [ false; true; true; true; true; true ];
    [ false; true; true; true; true; true ];
  ]

module Distance : SCALAR with type t = int = struct
  type t = int

  exception ScalarIllegal

  let zero = -1
  let one = 0

  let ( ++ ) x y =
    match (x, y) with -1, _ -> y | _, -1 -> x | _ -> Int.min x y

  let ( ** ) x y = if x = -1 || y = -1 then -1 else x + y
  let ( == ) x y = x = y
end

(* .. Write some code here .. *)

module DistanceMat = MatrixFn (Distance)
module DistanceMatClosure = ClosureFn (DistanceMat)

let distance g =
  try
    g |> DistanceMat.create |> DistanceMatClosure.closure |> DistanceMat.to_list
  with DistanceMat.MatrixIllegal -> raise IllegalFormat

let dl =
  [
    [ 0; -1; -1; -1; -1; -1 ];
    [ -1; 0; 35; 200; -1; -1 ];
    [ -1; 50; 0; -1; 150; -1 ];
    [ -1; 75; -1; 0; 100; 25 ];
    [ -1; -1; 50; 65; 0; -1 ];
    [ -1; -1; -1; -1; -1; 0 ];
  ]

let solution_dl' =
  [
    [ 0; -1; -1; -1; -1; -1 ];
    [ -1; 0; 35; 200; 185; 225 ];
    [ -1; 50; 0; 215; 150; 240 ];
    [ -1; 75; 110; 0; 100; 25 ];
    [ -1; 100; 50; 65; 0; 90 ];
    [ -1; -1; -1; -1; -1; 0 ];
  ]

module Weight : SCALAR with type t = int = struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = -1
  let ( ++ ) x y = if x = -1 || y = -1 then -1 else Int.max x y

  let ( ** ) x y =
    match (x, y) with
    | 0, _ | _, 0 -> 0
    | -1, _ -> y
    | _, -1 -> x
    | _ -> Int.min x y

  let ( == ) x y = x = y
end

(* .. Write some code here .. *)

module WeightMat = MatrixFn (Weight)
module WeightMatClosure = ClosureFn (WeightMat)

let weight g =
  try g |> WeightMat.create |> WeightMatClosure.closure |> WeightMat.to_list
  with WeightMat.MatrixIllegal -> raise IllegalFormat

let ml =
  [
    [ -1; 0; 0; 0; 0; 0 ];
    [ 0; -1; 10; 100; 0; 0 ];
    [ 0; 50; -1; 0; 150; 0 ];
    [ 0; 75; 0; -1; 125; 40 ];
    [ 0; 0; 25; -1; -1; 0 ];
    [ 0; 0; 0; 0; 0; -1 ];
  ]

let solution_ml' =
  [
    [ -1; 0; 0; 0; 0; 0 ];
    [ 0; -1; 25; 100; 100; 40 ];
    [ 0; 75; -1; 150; 150; 40 ];
    [ 0; 75; 25; -1; 125; 40 ];
    [ 0; 75; 25; -1; -1; 40 ];
    [ 0; 0; 0; 0; 0; -1 ];
  ]

let _ =
  try
    if
      reach al = solution_al'
      && distance dl = solution_dl'
      && weight ml = solution_ml'
    then print_endline "\nYour program seems fine (but no guarantee)!"
    else print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!"

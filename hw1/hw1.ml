exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum x = if x = 0 then 0 else x + sum (x - 1)
let rec power x n = if n = 0 then 1 else x * power x (n - 1)
let rec gcd x y = if x = 0 then y else gcd (y mod x) x

let rec combi n k =
  if k = 0 || k = n then 1
  else if 0 < k && k < n then combi (n - 1) k + combi (n - 1) (k - 1)
  else 0

let rec sum_tree t =
  match t with Leaf x -> x | Node (l, x, r) -> x + sum_tree l + sum_tree r

let rec depth t =
  match t with
  | Leaf _ -> 0
  | Node (l, _, r) ->
      let ld = 1 + depth l in
      let rd = 1 + depth r in
      if ld > rd then ld else rd

let rec bin_search t x =
  match t with
  | Leaf y -> x = y
  | Node (l, y, r) ->
      if x = y then true else if x < y then bin_search l x else bin_search r x

let rec postorder t =
  match t with
  | Leaf x -> [ x ]
  | Node (l, x, r) -> postorder l @ postorder r @ [ x ]

let rec max l =
  match l with
  | [] -> 0
  | x :: t ->
      let tmax = max t in
      if x > tmax then x else tmax

let rec list_add xs ys =
  match (xs, ys) with
  | [], _ -> ys
  | _, [] -> xs
  | x :: xt, y :: yt -> (x + y) :: list_add xt yt

let rec insert x ys =
  match ys with
  | [] -> [ x ]
  | y :: yt -> if x < y then x :: ys else y :: insert x yt

let rec insort xs =
  let rec insort_inner xs ys =
    match ys with [] -> xs | y :: yt -> insort_inner (insert y xs) yt
  in
  insort_inner [] xs

let rec compose f g = fun x -> g (f x) [@@ocamlformat "disable"]
let rec curry f x y = f (x, y)
let rec uncurry f (x, y) = f x y
let rec multifun f n = if n = 1 then f else compose f (multifun f (n - 1))

let rec ltake l n =
  match (l, n) with
  | _, 0 -> []
  | [], _ -> []
  | x :: t, _ -> x :: ltake t (n - 1)

let rec lall f l = match l with [] -> true | x :: t -> f x && lall f t
let rec lmap f l = match l with [] -> [] | x :: t -> f x :: lmap f t
let rec lrev l = match l with [] -> [] | x :: t -> lrev t @ [ x ]
let rec lflat l = match l with [] -> [] | x :: t -> x @ lflat t

let rec lzip xs ys =
  match (xs, ys) with
  | [], _ -> []
  | _, [] -> []
  | x :: xt, y :: yt -> (x, y) :: lzip xt yt

let rec split l =
  match l with
  | [] -> ([], [])
  | [ x ] -> ([ x ], [])
  | x :: y :: t ->
      let xs, ys = split t in
      (x :: xs, y :: ys)

let rec cartprod xs ys =
  match xs with
  | [] -> []
  | x :: xt -> lmap (fun y -> (x, y)) ys @ cartprod xt ys

let rec powerset s =
  match s with
  | [] -> [ [] ]
  | x :: xt ->
      let t = powerset xt in
      t @ lmap (fun l -> x :: l) t

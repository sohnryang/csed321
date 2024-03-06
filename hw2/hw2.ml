exception NotImplemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(** Recursive functions **)

let rec lrevrev l =
  let rec lrev l = match l with [] -> [] | x :: t -> lrev t @ [ x ] in
  match l with [] -> [] | x :: t -> lrevrev t @ [ lrev x ]

let rec lfoldl f e l = match l with [] -> e | x :: t -> lfoldl f (f (x, e)) t

(** Tail recursive functions  **)

let fact n =
  let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (acc * n) in
  fact_aux n 1

let fib n =
  let rec fib_aux n a b = if n = 0 then a else fib_aux (n - 1) b (a + b) in
  fib_aux (n + 1) 0 1

let alterSum l =
  let rec alterSum_aux l acc coeff =
    match l with
    | [] -> acc
    | x :: t -> alterSum_aux t (acc + (coeff * x)) (-coeff)
  in
  alterSum_aux l 0 1

let ltabulate n f =
  let rec ltabulate_aux n f l =
    if n < 0 then l else ltabulate_aux (n - 1) f (f n :: l)
  in
  ltabulate_aux (n - 1) f []

let lfilter p l =
  let rec lfilter_aux p l fl =
    match l with
    | [] -> fl
    | x :: t -> if p x then lfilter_aux p t (fl @ [ x ]) else lfilter_aux p t fl
  in
  lfilter_aux p l []

let union xs ys =
  let rec union_aux xs ys =
    match xs with
    | [] -> ys
    | x :: xt -> (
        match lfilter (fun y -> y = x) ys with
        | [] -> union_aux xt (x :: ys)
        | _ -> union_aux xt ys)
  in
  union_aux xs ys

let inorder t =
  let rec inorder_aux t post =
    match t with
    | Leaf x -> x :: post
    | Node (l, x, Leaf y) -> inorder_aux l (x :: y :: post)
    | Node (l, x, Node (l', y, r)) ->
        inorder_aux (Node (Node (l, x, l'), y, r)) post
  in
  inorder_aux t []

let postorder t =
  let rec postorder_aux ts post =
    match ts with
    | [] -> post
    | Leaf x :: tt -> postorder_aux tt (x :: post)
    | Node (l, x, r) :: tt -> postorder_aux (r :: l :: tt) (x :: post)
  in
  postorder_aux [ t ] []

let preorder t =
  let rec preorder_aux ts post =
    match ts with
    | [] -> post
    | Leaf x :: tt -> preorder_aux tt (post @ [ x ])
    | Node (l, x, r) :: tt -> preorder_aux (l :: r :: tt) (post @ [ x ])
  in
  preorder_aux [ t ] []

(** Sorting in the ascending order **)

let rec quicksort l =
  match l with
  | [] -> []
  | p :: t ->
      let lo, hi =
        lfoldl
          (fun (x, (lo, hi)) -> if x < p then (x :: lo, hi) else (lo, x :: hi))
          ([], []) t
      in
      quicksort lo @ [ p ] @ quicksort hi

let rec mergesort l =
  match l with
  | [] | [ _ ] -> l
  | _ ->
      let rec split l xs ys =
        match l with
        | [] -> (xs, ys)
        | [ x ] -> (x :: xs, ys)
        | x :: y :: t -> split t (x :: xs) (y :: ys)
      in
      let rec merge xs ys =
        match (xs, ys) with
        | xs, [] -> xs
        | [], ys -> ys
        | x :: xt, y :: yt ->
            if x < y then x :: merge xt ys else y :: merge xs yt
      in
      let xs, ys = split l [] [] in
      merge (mergesort xs) (mergesort ys)

(** Structures **)

module type HEAP = sig
  exception InvalidLocation

  type loc
  type 'a heap

  val empty : unit -> 'a heap
  val allocate : 'a heap -> 'a -> 'a heap * loc
  val dereference : 'a heap -> loc -> 'a
  val update : 'a heap -> loc -> 'a -> 'a heap
end

module type DICT = sig
  type key
  type 'a dict

  val empty : unit -> 'a dict
  val lookup : 'a dict -> key -> 'a option
  val delete : 'a dict -> key -> 'a dict
  val insert : 'a dict -> key * 'a -> 'a dict
end

module Heap : HEAP = struct
  exception InvalidLocation

  type loc = int
  type 'a heap = 'a list * int

  let empty () = ([], 0)

  let allocate h v =
    let storage, size = h in
    ((v :: storage, size + 1), size)

  let dereference h l =
    let storage, size = h in
    if l >= size then raise InvalidLocation else List.nth storage l

  let update h l v =
    let storage, size = h in
    if l >= size then raise InvalidLocation
    else
      let new_storage, _ =
        List.fold_right
          (fun x (new_storage, i) ->
            ((if i == l then v else x) :: storage, i + 1))
          storage ([], 0)
      in
      (new_storage, size)
end

module DictList : DICT with type key = string = struct
  type key = string
  type 'a dict = (key * 'a) list

  let empty () = []

  let lookup d k =
    match List.find_opt (fun (k', _) -> k' = k) d with
    | Some (_, v) -> Some v
    | None -> None

  let delete d k = List.filter (fun (k', _) -> k' <> k) d

  let insert d (k, v) =
    let d' = delete d k in
    (k, v) :: d'
end

module DictFun : DICT with type key = string = struct
  type key = string
  type 'a dict = key -> 'a option

  let empty () = (fun _ -> None) [@@ocamlformat "disable"]
  let lookup d k = d k

  let delete d k = fun k' -> if k' = k then None else d k' [@@ocamlformat "disable"]

  let insert d (k, v) = fun k' -> if k' = k then Some v else d k' [@@ocamlformat "disable"]
end

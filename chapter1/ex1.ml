(* chapter 1 excecise 1.1 *)
module Tree = struct
type key = string
type tree = Leaf | Tree of tree * key * tree

let empty = Leaf

let rec insert key tree =
  match tree with
  | Leaf -> Tree (Leaf, key, Leaf)
  | Tree(left, k, right) ->
    if key < k then Tree (insert key left, k, right) else
    if key = k then Tree (left, k, right) else
      Tree (left, k, insert key right)

(* 1.1 a *)

let rec member key tree =
  match tree with
  | Leaf -> false
  | Tree(left, k, right) ->
    if key < k then member key left else
    if key > k then member key right else
    true
end

(* 1.1 b *)

module ExtendTree = struct
  type key = string
  type 'a tree = Leaf | Tree of ('a tree) * key * 'a * ('a tree)

  let rec insert key value tree =
    match tree with
    | Leaf -> Tree(Leaf, key, value, Leaf)
    | Tree(left, k, v, right) ->
      if k > key then Tree(insert key value left, k, v, right) else
      if k < key then Tree(left, k, v, insert key value right) else
        Tree(left, k, v, right)

  let rec lookup tree key =
    match tree with
    | Leaf -> None
    | Tree(left, k, v, right) ->
      if k > key then lookup left key else
      if k < key then lookup right key else
        Some v
end

(* 1.1 c *)

(*
t  a
/  \
s  b
/  \
p  c
/  \
i  d
/  \
.. ..
*)

(* 1.1 d *)

module TreeBinary =
struct
type key = string
type 'a node = key * 'a
type 'a tree = Leaf | Node of 'a node * 'a tree * 'a tree

let empty = Leaf

let rec insert key value tree =
  match tree with
    | Leaf -> Node((key, value), Leaf, Leaf)
    | Node((k, v), left, right) ->
      if key < k then
        Node((k, v), (insert key value left), right)
      else
        Node((k, v), left, (insert key value right))

let rec delete key tree =
  match tree with
    | Leaf -> Leaf
    | Node((k, v), left, right) ->
      if key < k then
        Node((k, v), (delete key left), right)
      else if k = key then
        match right with
          | Leaf -> (
            match left with
              | Leaf -> Leaf
              | Node(nl, ll, lr) ->
                let (precessor, leftBeDeleted) = deleteBiggest nl ll lr in
                  Node(precessor, leftBeDeleted, right)
          ) | Node(nr, rl, rr) ->
            let (successor, rightBeDeleted) = deleteSmallest nr rl rr in
            Node(successor, left, rightBeDeleted)
      else
        Node((k, v), left, (delete key right))
and deleteSmallest n left right =
  match left with
    | Leaf -> (n, right)
    | Node(nl, ll, lr) ->
      let (suc, rBeDeleted) = deleteSmallest nl ll lr in
      (suc, Node(n, rBeDeleted,right))
and deleteBiggest n left right =
  match right with
    | Leaf -> (n, left)
    | Node(nr, rl, rr) ->
      let (pre, lBeDeleted) = deleteBiggest nr rl rr in
      (pre, Node(n, left, lBeDeleted))

let from_int_list list =
  let foldee tree value = insert (string_of_int value) value tree in
    List.fold_left foldee empty list

end

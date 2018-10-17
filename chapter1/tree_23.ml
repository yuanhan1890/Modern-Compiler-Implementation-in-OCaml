(* 2-3-tree *)

module Tree23 =
struct
type key = int
type 'a node = key * 'a
type 'a tree =
  Leaf |
  Node2 of 'a node * 'a tree * 'a tree | (* two children *)
  Node3 of 'a node * 'a node * 'a tree * 'a tree * 'a tree (* three children *)

type insertPos2 = Left2 | Right2
type insertPos3 = Left3 | Middle3 | Right3

type 'a kickNode =
  | Done of 'a tree
  | Up2 of 'a node * 'a tree* 'a tree
  | Up3 of 'a node * 'a node * 'a tree * 'a tree * 'a tree
let empty = Leaf

let kickup2_left (k, v) lKickupNode right =
  match lKickupNode with
    | Done(left) -> Up2((k, v), left, right)
    | Up2(nl, ll, lr) ->
      Done(Node3(nl, (k, v), ll, lr, right))
    | Up3(nl1, nl2, ll, lm, lr) ->
      Up2(nl2, Node2(nl1, ll, lm), Node2((k, v), lr, right))

let kickup2_right (k, v) left rKickupNode =
  match rKickupNode with
    | Done(right) -> Done(Node2((k, v), left, right))
    | Up2((kr, vr), rl, rr) ->
      Done(Node3((k, v), (kr, vr), left, rl, rr))
    | Up3((kr1, vr1), (kr2, vr2), rl, rm, rr) ->
      Up2((kr1, vr1), Node2((k, v), left, rl), Node2((kr2, vr2), rm, rr))

let kickup3_left n1 n2 lKickNode middle right =
  match lKickNode with
    | Done(left) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nl, ll, lr) ->
      Up2(n1, Node2(nl, ll, lr), Node2(n2, middle, right))
    | Up3((nl1, nl2, ll, lm, lr)) ->
      Up2(n1, Node3(nl1, nl2, ll, lm, lr), Node2(n2, middle, right))

let kickup3_middle n1 n2 left mKickNode right =
  match mKickNode with
    | Done(middle) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nm, ml, mr) ->
      Up2(nm, Node2(n1, left, ml), Node2(n2, mr, right))
    | Up3((nm1, nm2, ml, mm, mr)) ->
      Up2(nm1, Node2(n1, left, ml), Node3(nm2, n2, mm, mr, right))
let kickup3_right n1 n2 left middle rKickNode =
  match rKickNode with
    | Done(right) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nr, rl, rr) ->
      Up2(n2, Node2(n1, left, middle), Node2(nr, rl, rr))
    | Up3((nr1, nr2, rl, rm, rr)) ->
      Up2(nr1, Node3(n1, n1, left, middle, rl), Node2(nr2, rm, rr))

let rec insert_aux key value tree =
  match tree with
    | Leaf -> Up2((key, value), Leaf, Leaf)
    | Node2((k, v), left, right) ->
      if key < k then
        kickup2_left (k, v) (insert_aux key value left) right
      else
        kickup2_right (k, v) left (insert_aux key value right)
    | Node3((k1, v1), (k2, v2), left, middle, right) ->
      if key < k1 then
        kickup3_left (k1, v1) (k2, v2) (insert_aux key value left) middle right
      else if key < k2 then
        kickup3_middle (k1, v1) (k2, v2) left (insert_aux key value middle) right
      else
        kickup3_right (k1, v1) (k2, v2) left middle (insert_aux key value right)

let insert key value tree =
  match insert_aux key value tree with
    | Done(tree) -> tree
    | Up2(n1, left, right) -> Node2(n1, left, right)
    | Up3(n1, n2, left, middle, right) -> Node3(n1, n2, left, middle, right)

let delete key tree = tree

let rec lookup key tree =
  match tree with
    | Leaf -> None
    | Node2((k, v), left, right) ->
      if key < k then lookup key left else
      if key = k then Some v else
        lookup key right
    | Node3((k1, v1), (k2, v2), left, middle, right) ->
      if key < k1 then lookup key left else
      if key = k1 then Some v1 else
      if key < k2 then lookup key middle else
      if key = k2 then Some v2 else
        lookup key right

let member key tree =
  match lookup key tree with
    | None -> false
    | Some v -> true

let from_int_list list =
  let mapper tree v = insert v v tree in
    List.fold_left mapper Leaf list

let checkBalanced tree =
  let rec _loop tree path_len collect_list =
    let nextPathLen = path_len + 1 in
      match tree with
        | Leaf -> (path_len + 1) :: collect_list
        | Node2(n1, c1, c2) ->
          List.append (_loop c1 nextPathLen collect_list) (_loop c2 nextPathLen collect_list)
        | Node3(n1, n2, c1, c2, c3) ->
          List.append
            (List.append
              (_loop c1 nextPathLen collect_list)
              (_loop c2 nextPathLen collect_list)
            )
            (_loop c3 nextPathLen collect_list) in
  let collected = _loop tree 0 [] in
  match collected with
    | [] -> true
    | head :: tail ->
      let checkEqual n = n = head in
        List.for_all checkEqual collected

end

let tree1 = Tree23.Node2((1, 1), Tree23.Leaf, Tree23.Leaf)
let tree2 = Tree23.from_int_list [1;2;3;4;5;6;7;8;9;10]

let checkBalanced tree prefix =
  match Tree23.checkBalanced tree with
    | true -> print_endline (prefix ^ " balanced")
    | false -> print_endline (prefix ^ " not balanced")

let main () =
  checkBalanced Tree23.empty "empty";
  checkBalanced tree1 "level1";;
  checkBalanced tree2 "from_list";;

main ();;

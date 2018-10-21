(* 2-3-tree *)
module Tree23 = (struct
type key = int
type 'a node = key * 'a
type 'a tree =
  Leaf |
  Node2 of 'a node * 'a tree * 'a tree | (* two children *)
  Node3 of 'a node * 'a node * 'a tree * 'a tree * 'a tree (* three children *)

type 'a kickNode =
  | Done of 'a tree
  | Up2 of 'a node * 'a tree* 'a tree

let empty = Leaf

let kickup2_left (k, v) lKickupNode right =
  match lKickupNode with
    | Done(left) -> Up2((k, v), left, right)
    | Up2(nl, ll, lr) ->
      Done(Node3(nl, (k, v), ll, lr, right))

let kickup2_right (k, v) left rKickupNode =
  match rKickupNode with
    | Done(right) -> Done(Node2((k, v), left, right))
    | Up2((kr, vr), rl, rr) ->
      Done(Node3((k, v), (kr, vr), left, rl, rr))

let kickup3_left n1 n2 lKickNode middle right =
  match lKickNode with
    | Done(left) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nl, ll, lr) ->
      Up2(n1, Node2(nl, ll, lr), Node2(n2, middle, right))

let kickup3_middle n1 n2 left mKickNode right =
  match mKickNode with
    | Done(middle) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nm, ml, mr) ->
      Up2(nm, Node2(n1, left, ml), Node2(n2, mr, right))
let kickup3_right n1 n2 left middle rKickNode =
  match rKickNode with
    | Done(right) -> Done(Node3(n1, n2, left, middle, right))
    | Up2(nr, rl, rr) ->
      Up2(n2, Node2(n1, left, middle), Node2(nr, rl, rr))

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


type up_case = Single | Done

let rec delete key tree =
  let (_, t) = delete_aux key tree in
  t
and delete_aux key tree =
  match tree with
    | Leaf -> (Done, Leaf)
    | Node2(n, l, r) ->
      let (k, _) = n in
      if key = k then
        match l with
          | Leaf -> (Single, Leaf)
          | Node2(nl, ll, lr) ->
            let (prev, new_l) = find_max_2 nl ll lr in
              collect_2_l prev new_l r
          | Node3(nl1, nl2, ll, lm, lr) ->
            let (prev, new_l) = find_max_3 nl1 nl2 ll lm lr in
              collect_2_l prev new_l r
      else if key < k then
        collect_2_l n (delete_aux key l) r
      else
        collect_2_r n l (delete_aux key r)
    | Node3(n1, n2, l, m, r) ->
      let (k1, _) = n1 in
      let (k2, _) = n2 in
      if key < k1 then
        collect_3_l n1 n2 (delete_aux key l) m r
      else if key = k1 then
        match l with
          | Leaf -> (Done, Node2(n2, Leaf, Leaf))
          | Node2(n1, l1, r1) ->
            let (n_, t_) = find_max_2 n1 l1 r1 in
              collect_3_l n_ n2 t_ m r
          | Node3(n1, n2, c1, c2, c3) ->
            let (n_, t_) = find_max_3 n1 n2 c1 c2 c3 in
              collect_3_l n_ n2 t_ m r
      else if key < k2 then
        collect_3_m n1 n2 l (delete_aux key m) r
      else if key = k2 then
        match m with
          | Leaf -> (Done, Node2(n1, Leaf, Leaf))
          | Node2(n1, l1, r1) ->
            let (n_, t_) = find_max_2 n1 l1 r1 in
              collect_3_m n1 n_ l t_ r
          | Node3(n1, n2, c1, c2, c3) ->
            let (n_, t_) = find_max_3 n1 n2 c1 c2 c3 in
              collect_3_m n1 n_ l t_ r
      else
        collect_3_r n1 n2 l m (delete_aux key r)
and find_max_2 n l r =
  match r with
    | Leaf -> (n, (Single, Leaf))
    | Node2(n1, l1, r1) ->
      let (n_, t_) = find_max_2 n1 l1 r1 in
      (n_, collect_2_r n l t_)
    | Node3(n1, n2, l1, m1, r1) ->
      let (n_, t_) = find_max_3 n1 n2 l1 m1 r1 in
      (n_, collect_2_r n r t_)
and find_max_3 n1 n2 l m r =
  match r with
    | Leaf -> (n1, (Done, Node2(n2, Leaf, Leaf)))
    | Node2(n1, l1, r1) ->
      let (n_, t_) = find_max_2 n1 l1 r1 in
      (n_, collect_3_r n1 n2 l m t_)
    | Node3(n1, n2, l1, m1, r1) ->
      let (n_, t_) = find_max_3 n1 n2 l1 m1 r1 in
      (n_, collect_3_r n1 n2 l m t_)
and collect_2_l n (up_case, l) r =
  match up_case with
    | Done -> (Done, Node2(n, l , r))
    | Single ->
      match r with
        | Leaf -> (Done, Node2(n, Leaf, Leaf))
        | Node2(nr, rl, rr) ->
          (Single, Node3(
            n, nr,
            l, rl, rr
          ))
        | Node3(nr1, nr2, rl, rm, rr) ->
          (Done, Node2(
            nr1,
            Node2(n, l, rl),
            Node2(nr2, rm, rr)
          ))
and collect_2_r n l (up_case, r) =
  match up_case with
    | Done -> (Done, Node2(n, l , r))
    | Single ->
      match l with
        | Leaf -> (Done, Node2(n, Leaf, Leaf))
        | Node2(nl, ll, lr) ->
          (Single, Node3(
            nl, n,
            ll, lr, r
          ))
        | Node3(nl1, nl2, ll, lm, lr) ->
          (Done, Node2(
            nl2,
            Node2(nl1, ll, lm),
            Node2(n, lr, r)
          ))
and collect_3_l n1 n2 (up_case, l) m r =
  match up_case with
    | Done -> (Done, Node3(n1, n2, l, m, r))
    | Single ->
      (Done, (
        match m with
        | Leaf -> Node3(n1, n2, Leaf, Leaf, Leaf)
        | Node2(nm, ml, mr) ->
          Node2(
            nm,
            Node3(n1, nm, l, ml, mr),
            r
          )
        | Node3(nm1, nm2, ml, mm, mr) ->
          Node3(
            nm1, n2,
            Node2(n1, l, ml),
            Node2(nm2, mm, mr),
            r
          )
      ))
and collect_3_m n1 n2 l (up_case, m) r =
  match up_case with
    | Done -> (Done, Node3(n1, n2, l, m, r))
    | Single -> (Done, (
      match l with
        | Leaf -> Node3(n1, n2, Leaf, Leaf, Leaf)
        | Node2(nl, ll, lr) ->
          Node2(n1,
            Node3(nl, n2, ll, lr, m),
            r
          )
        | Node3(nl1, nl2, ll, lm, lr) ->
          Node3(nl2, n2,
            Node2(nl1, ll, lm),
            Node2(n1, lr, m),
            r)
    ))
and collect_3_r n1 n2 l m (up_case, r) =
  match up_case with
    | Done -> (Done, Node3(n1, n2, l, m, r))
    | Single ->
      (Done, (
        match m with
        | Leaf -> Node3(n1, n2, Leaf, Leaf, Leaf)
        | Node2(nm, ml, mr) ->
          Node2(
            n1,
            l,
            Node3(nm, n2, ml, mr, r)
          )
        | Node3(nm1, nm2, ml, mm, mr) ->
          Node3(
            n1, nm2,
            l,
            Node2(nm1, ml, mm),
            Node2(n2, mr, r)
          )
      ))

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
    | Some _ -> true

let rec invariant tree =
  let check_left key tree = ((
    match tree with
      | Leaf -> true
      | Node2((k, _), _, _) ->
         (key > k)
      | Node3((k1, _), (k2, _), _, _, _) ->
        (key > k1) && (key > k2) && (k2 > k1)
  ) && invariant tree) in
  let check_right key tree = ((
    match tree with
      | Leaf -> true
      | Node2((k, _), _, _) ->
        (key < k)
      | Node3((k1, _), (k2, _), _, _, _) ->
        (key < k1) && (key < k2) && (k2 > k1)
  ) && invariant tree) in
  match tree with
    | Leaf -> true
    | Node2((k, _), l, r) ->
      (check_left k l) && (check_right k r)
    | Node3((k1, _), (k2, _), l, m, r) ->
      (check_left k1 l) && (check_right k1 m) && (check_left k2 m) && (check_right k2 r)

end : Tree.TreeBalanced with type key = int)

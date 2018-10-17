let max x y = if x < y then y else x

module TreeAvl =
struct
type key = int
type 'a node = key * 'a
type height = int
type 'a tree =
  | Leaf
  | Node of 'a node * 'a tree * 'a tree

let rec get_height tree =
  match tree with
  | Leaf -> 0
  | Node(n, l, r) -> 1 + (max (get_height l) (get_height r))

let rec insert key value tree =
  let (n, l, r) = insert_child key value tree in
  Node(n, l, r)
and insert_child key value tree =
  let n = (key, value) in
  match tree with
    | Leaf -> (n, Leaf, Leaf)
    | Node(n0, l0, r0) ->
      let (k0, _) = n0 in
      let hl0 = get_height l0 in
      let hr0 = get_height r0 in
      if key < k0 then (
        match l0 with
        | Leaf -> (n0, Node(n, Leaf, Leaf), r0)
        | Node(n1, l1, r1) ->
          let (k1, _) = n1 in
          if (hl0 < hr0) || (hl0 = hr0) then
            let l0_new = insert key value l0 in
              (n0, l0_new, r0)
          else
            match r0 with
              | Leaf ->
                  if key < k1 then
                    (n1, Node(n, Leaf, Leaf), Node(n0, Leaf, Leaf))
                  else
                    (n, Node(n1, Leaf, Leaf), Node(n0, Leaf, Leaf))
              | Node _ ->
                  let hr0 = get_height r0 in
                  let toBeInserted = if key < k1 then l1 else r1 in
                  let (n_new, l_new, r_new) = insert_child key value toBeInserted in
                  let h_new = (max (get_height l_new) (get_height r_new)) + 2 in
                  if key < k1 then
                    if (h_new - hr0) > 1 then
                      (n1,
                        Node(n_new, l_new, r_new),
                        Node(n0, r1, r0))
                    else
                      (n0,
                        Node(
                          n1,
                          Node(n_new, l_new, r_new),
                          r1
                        ),
                        r0)
                  else
                    if (h_new - hr0) > 1 then
                      (n_new,
                        Node(n1, l1, l_new),
                        Node(n0, r_new, r0))
                    else
                      (n0,
                        Node(
                          n1,
                          l1,
                          Node(n_new, l_new, r_new)
                        ),
                        r0)
        )
      else (
        match r0 with
        | Leaf -> (n0, l0, Node(n, Leaf, Leaf))
        | Node(n1, l1, r1) ->
          let (k1, _) = n1 in
          if (hl0 > hr0) || (hl0 = hr0) then
            let r0_new = insert key value r0 in
              (n0, l0, r0_new)
          else
            match l0 with
              | Leaf ->
                  if key < k1 then
                    (n, Node(n0, Leaf, Leaf), Node(n1, Leaf, Leaf))
                  else
                    (n1, Node(n0, Leaf, Leaf), Node(n, Leaf, Leaf))
              | Node _ ->
                  let hl0 = get_height l0 in
                  let toBeInserted = if key < k1 then l1 else r1 in
                  let (n_new, l_new, r_new) = insert_child key value toBeInserted in
                  let h_new = (max (get_height l_new) (get_height r_new)) + 2 in
                  if key < k1 then
                    if (h_new - hl0) > 1 then
                      (n_new,
                        Node(n0, l0, l_new),
                        Node(n1, r_new, r1))
                    else
                      (n0,
                        l0,
                        Node(n1,
                          Node(n_new, l_new, r_new),
                          r1
                        ))
                  else
                    if (h_new - hl0) > 1 then
                      (n1,
                        Node(n0, l0, l1),
                        Node(n_new, l_new, r_new))
                    else
                      (n0,
                        l0,
                        Node(n1, l1, Node(n_new, l_new, r_new)))

      )

let empty = Leaf

let invariant arr tree =
  let max i j = if i > j then i else j in
  let legal_left_key key = function
    | Leaf -> true
    | Node((kl, _), _, _) ->
      assert (key > kl);
      (key > kl)
  in
  let legal_right_key key = function
    | Leaf -> true
    | Node((kr, _), _, _) ->
      assert (key < kr);
      (key < kr)
  in
  let rec inv = function
    | Leaf -> true
    | Node(n, l, r) as tree ->
      let (key, _) = n in
      let (hl, hr) = (get_height l, get_height r) in
      let h = get_height tree in
      print_endline ((string_of_int hl) ^ " " ^ (string_of_int hr) ^ " " ^ (string_of_int h));
      if (inv l) <> true then
        false
      else if (inv l) <> true then
        false
      else if (legal_left_key key l) <> true then
        false
      else if (legal_right_key key r) <> true then
        false
      else if h <> (max hl hr) + 1 then
        begin
          false
        end
      else if abs (hl - hr) >= 2 then
        false
      else
        true
  in
    if inv tree then None else Some arr
end

let from_ints list =
  List.fold_left (fun tree i -> (TreeAvl.insert i i tree)) TreeAvl.empty list

let build_list_base n =
  let rec aux acc i =
    if i <= n then
      aux (i::acc) (i+1)
    else (List.rev acc)
  in
  aux [] 1

let ran_sorted x y = if (Random.float 1.0) < 0.5 then 1 else (-1)

let build_list len random reversed =
  let arr = build_list_base len in
    if random then
      List.sort ran_sorted arr
    else if reversed then
      List.rev arr
    else
      arr

let build_suite lens =
  List.map (fun i -> (
      let arr = (build_list i false false) in
        (TreeAvl.invariant arr (from_ints arr)),
      let arr = (build_list i true false) in
        (TreeAvl.invariant arr (from_ints arr)),
      let arr = (build_list i false true) in
        (TreeAvl.invariant arr (from_ints arr)),
      let arr = (build_list i true true) in
        (TreeAvl.invariant arr (from_ints arr))
    )) lens

let test arr =
  TreeAvl.invariant arr (from_ints arr);
(* build_suite [100]; *)
(* (from_ints [7; 8; 6; 9; 45; 50; 48; 12; 74; 64; 65; ]) *)

open OUnit
open Chapter1
open Chapter1.Tree_23

module TestTree =
functor (Tree: Tree.TreeBalanced with type key = int) ->
struct
(* build tree from int list*)
let from_ints list =
  List.fold_left (fun tree i -> (Tree.insert i i tree)) Tree.empty list

(* build int list from 1 to n increment by 1 *)
let build_list_base n =
  let rec aux acc i =
    if i <= n then
      aux (i::acc) (i+1)
    else (List.rev acc)
  in
  aux [] 1

(* random sorter for random sort *)
let ran_sorted _ _ = if (Random.float 1.0) < 0.5 then 1 else (-1)

(* build int list with length 'len', can be random or reversed or both attr *)
let build_list len random reversed =
  let arr = build_list_base len in
    if random then
      List.sort ran_sorted arr
    else if reversed then
      List.rev arr
    else
      arr

(* delete a random member from array, return deleted member and remain array *)
let random_delete arr =
    let i = Random.int (List.length arr) in
      (List.nth arr i, List.fold_left (fun arr x ->
        match x with
          | None -> arr
          | Some x -> x :: arr
      ) [] (List.mapi (fun index x -> if index = i then None else Some x) arr))

(* delete member in arr from tree in order *)
let delete_tree arr tree =
  List.fold_left (fun tree i -> Tree.delete i tree) tree arr

(* choose members from array, and use it to delete from tree *)
let delete_test arr tree =
  let len = List.length arr in
  let delete_num = Random.int len in
  let rec delete_iter arr accum count =
    match count with
      | 0 -> accum
      | _ ->
        let (select_idx, remain) = random_delete arr in
        delete_iter remain (select_idx :: accum) (count - 1)
  in
  let select_ids = delete_iter arr [] delete_num in
  (select_ids, delete_tree select_ids tree)


type 'a suit_fail = Suc | Insert of 'a list | Delete of 'a list * 'a list

let build_suite lens =
  List.map (fun i -> (
      (let arr = (build_list i false false) in
      let t = (from_ints arr) in
      let (ids, t_d) = (delete_test arr t) in
        if false = (Tree.invariant t) then Insert arr
        else if false = (Tree.invariant t_d) then Delete(arr, ids)
        else Suc),
      (let arr = (build_list i true false) in
      let t = (from_ints arr) in
      let (ids, t_d) = (delete_test arr t) in
        if false = (Tree.invariant t) then Insert arr
        else if false = (Tree.invariant t_d) then Delete(arr, ids)
        else Suc),
      (let arr = (build_list i false true) in
      let t = (from_ints arr) in
      let (ids, t_d) = (delete_test arr t) in
        if false = (Tree.invariant t) then Insert arr
        else if false = (Tree.invariant t_d) then Delete(arr, ids)
        else Suc),
      let arr = (build_list i true true) in
      let t = (from_ints arr) in
      let (ids, t_d) = (delete_test arr t) in
        if false = (Tree.invariant t) then Insert arr
        else if false = (Tree.invariant t_d) then Delete(arr, ids)
        else Suc
    )) lens
end

module Test23 = TestTree(Tree23)

let test_tree =
  let arr = Test23.build_list 10 true true in
  let tree = (Test23.from_ints arr) in
  let (_, tree_deleted) = (Test23.delete_test arr tree) in
  "2-3-tree insert normal" >:: (fun _ -> begin
    assert_equal true (Tree23.invariant tree);
    assert_equal true (Tree23.invariant tree_deleted);
  end)

let _ =
  run_test_tt test_tree

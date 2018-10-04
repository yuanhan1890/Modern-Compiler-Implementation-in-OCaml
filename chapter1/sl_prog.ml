(* straight line program typedefs *)

type id = string
type binop = Plus | Minus | Times | Div

type stm =
  CompoundStm of stm * stm |
  AssignStm of id * exp |
  PrintStm of exp list
and exp =
  IdExp of id |
  NumExp of int |
  OpExp of exp * binop * exp |
  EseqExp of stm * exp

(*
  a := 5 * 3; b := (print(a, a - 1), a * 10); print(b)
*)
let prog =
  CompoundStm(
    AssignStm(
      "a",
      OpExp(
        NumExp 5,
        Plus,
        NumExp 3
      )
    ),
    CompoundStm(
      AssignStm(
        "b",
        EseqExp(
          PrintStm([
            IdExp "a";
            OpExp(
              IdExp "a",
              Minus,
              NumExp 1
            )
          ]),
          OpExp(
            IdExp "a",
            Times,
            NumExp 10
          )
        )
      ),
      PrintStm([
        IdExp "b"
      ])
    )
  )

(* program 1 *)

let maxInts a b =
  match a - b > 0 with
    | true -> a
    | false -> b

let rec maxargs (prog: stm) =
  match prog with
    | CompoundStm(stmA, stmB) -> maxInts (maxargs stmA) (maxargs stmB)
    | AssignStm(id, exp) -> (
      match exp with
        | EseqExp(stm, exp) -> maxargs stm
        | _ -> 0
      )
    | PrintStm(expList) -> List.length expList

let main () =
  print_int (maxargs prog);;
  print_newline();;

main();;

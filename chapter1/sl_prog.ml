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

(* program 2 *)

exception Evaluate_Fail
exception GET_UNDEFINED_VALUE of string

module ProgEnv =
struct
  type envs = (string * int) list
  let empty: envs = []
  let assign id value (envs: envs) =
    (id, value) :: envs
  let rec get id (envs: envs) =
    match envs with
      | [] -> None
      | (ident, value) :: rest ->
        if ident = id then Some value else get id rest
end

let rec interpExp: exp -> ProgEnv.envs -> (int * ProgEnv.envs) =
  fun (expression: exp) (envs: ProgEnv.envs) ->
    match expression with
      | IdExp(id) ->
        let value = (
          match ProgEnv.get id envs with
            | None -> raise (GET_UNDEFINED_VALUE id)
            | Some value -> value
        ) in (value, envs)
      | NumExp(intValue) -> (intValue, envs)
      | OpExp(expA, binop, expB) ->
        let (expAValue, envA) = interpExp expA envs in
        let (expBValue, envB) = interpExp expB envA in
        let finalValue = match binop with
          | Plus -> expAValue + expBValue
          | Minus -> expAValue - expBValue
          | Times -> expAValue * expBValue
          | Div -> expAValue / expBValue in
          (finalValue, envB)
      | EseqExp(stm, exp) ->
        let newEnvs = interpStm stm envs in
          interpExp exp newEnvs
and interpStm (statement: stm) (envs: ProgEnv.envs) =
  match statement with
    | CompoundStm(stmA, stmB) ->
      let newEnvs = interpStm stmA envs in
        interpStm stmB newEnvs
    | PrintStm(expList) ->
      let iterExpsFunc envs expression =
        let (value, newEnvs) = interpExp expression envs in
          print_int(value);
          print_string " ";
          newEnvs
      in let newEnvs = List.fold_left iterExpsFunc envs expList in
        print_newline();
        newEnvs;
    | AssignStm(id, exp) ->
      let (value, newEnvs) = interpExp exp envs in
        ProgEnv.assign id value newEnvs

let interp (prog: stm) = interpStm prog ProgEnv.empty

let main () =
  print_int (maxargs prog);;
  print_newline();;
  interp prog;;

main();;

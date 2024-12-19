open Utils

(* Register A: 66245665
Register B: 0
Register C: 0

Program: 2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0 *)

let get_register s =
  let r = Str.regexp {|Register \([A-C]\): \([0-9]+\)|} in 
  match get_all_groups r s with
  | [(_, [_; v])] -> int_of_string v
  | _ -> failwith ("Invalid input: "^s)

let get_nums s =
  let r = Str.regexp {|[0-9]+|} in
  get_all_groups r s
  |> List.map (function (x, []) -> int_of_string x | _ -> failwith ("Invalid input: "^s))

let data = 
  "inputs/17.txt"
  |> read_lines

let ra = get_register (List.nth data 0)
let rb = get_register (List.nth data 1)
let rc = get_register (List.nth data 2)

let program = get_nums (List.nth data 4)

let execute op arg (a,b,c) =
  let combo_arg = 
    match arg with
    | 4 -> a
    | 5 -> b
    | 6 -> c
    (* | 7 -> failwith "Invalid argument" *)
    | _ -> arg
  in
  let pow x n = int_of_float (float_of_int x ** float_of_int n) in
  match op with
  | 0 -> 
    (a/(pow 2 combo_arg), b, c),None,[]
  | 1 ->
    (a,b lxor arg, c),None,[]
  | 2 ->
    (a, combo_arg mod 8, c),None,[]
  | 3 ->
    if a = 0 then (a,b,c),None,[] else
    (a,b,c),Some arg, []
  | 4 -> 
    (a,b lxor c,c),None, []
  | 5 -> 
    (a,b,c),None,[combo_arg mod 8]
  | 6 ->
    (a,a/(pow 2 combo_arg),c),None,[]
  | 7 ->
    (a,b,a/(pow 2 combo_arg)),None,[]
  | _ -> failwith "Invalid operation"

let rec run_program p (pos,regs) =
  if pos >= List.length p then
    []
  else
    let op = List.nth p pos in
    let arg = List.nth p (pos+1) in
    let regs',jump,output = execute op arg regs in
    output @
    match jump with
    | Some n -> run_program p (n,regs')
    | None ->  run_program p (pos+2,regs')
  
let () =
    let os = run_program program (0,(ra,rb,rc)) in
    Printf.printf "Part 1: %s\n" (String.concat "," (List.map string_of_int os))

(* Program: 
2,4, B=A%8
1,7, B=B xor 7
7,5, C=A/2^B
1,7, B=B xor 7
4,6, B=B xor C
0,3, A=A/2^3
5,5, out B%8
3,0  repeat until A is zero 


c=A/2^(!b)
B=a xor c
*)

(* 2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0 *)
let () =
  let a = Z3.BitVector.mk_const_s Smt.ctx "A" 64 in
  let compute a = 
    (* let b = Z3.BitVector.mk_const_s Smt.ctx "B" 64 in
    let c = Z3.BitVector.mk_const_s Smt.ctx "C" 64 in
    let out, regs = execute_symbolic program (a,b,c) in *)
    let b = Z3.BitVector.mk_smod Smt.ctx a (Z3.BitVector.mk_numeral Smt.ctx "8" 64) in
    (* let b = Z3.BitVector.mk_xor Smt.ctx b (Z3.BitVector.mk_numeral Smt.ctx "7" 64) in
    let c = Z3.BitVector.mk_udiv Smt.ctx a (Z3.BitVector.mk_shl Smt.ctx (Z3.BitVector.mk_numeral Smt.ctx "1" 64) b) in
    let b = Z3.BitVector.mk_xor Smt.ctx b (Z3.BitVector.mk_numeral Smt.ctx "7" 64) in *)
    let b2 = Z3.BitVector.mk_xor Smt.ctx b (Z3.BitVector.mk_numeral Smt.ctx "7" 64) in
    let c = Z3.BitVector.mk_udiv Smt.ctx a (Z3.BitVector.mk_shl Smt.ctx (Z3.BitVector.mk_numeral Smt.ctx "1" 64) b2) in
    let b = Z3.BitVector.mk_xor Smt.ctx b c in
    let a = Z3.BitVector.mk_udiv Smt.ctx a (Z3.BitVector.mk_numeral Smt.ctx "8" 64) in
    let digit = Z3.BitVector.mk_urem Smt.ctx b (Z3.BitVector.mk_numeral Smt.ctx "8" 64) in
    digit, a
  in
  (* let digit1, a = compute a in *)
  let a_rem,constraints = List.fold_left (fun (a,constraints) goal_digit ->
    let digit, a = compute a in
    (a, 
    (Z3.Boolean.mk_eq Smt.ctx digit (Z3.BitVector.mk_numeral Smt.ctx (string_of_int goal_digit) 64))::constraints
    )
  ) (a,[]) [2;4;1;7;7;5;1;7;4;6;0;3;5;5;3;0] in

  let eval_bv m bv = Z3.Model.eval m bv true |> Option.get |> Z3.BitVector.numeral_to_string in

  let rec find_min upper_bound =
  Smt.eval_model
  (
      (match upper_bound with
      | Some ub -> [Z3.BitVector.mk_ult Smt.ctx a (Z3.BitVector.mk_numeral Smt.ctx ub 64)]
      | None -> [])
  @(
    Z3.Boolean.mk_eq Smt.ctx a_rem (Z3.BitVector.mk_numeral Smt.ctx "0" 64)
  )::constraints)
  (* (a) *)
  (function
    | Ok m -> find_min (Some (eval_bv m a))
    | Error _ -> upper_bound
  )
  in
  (*
  7min with optimization
  6s with recursion
  *)
  find_min None
  |> Option.get
  |> Printf.printf "Part 2: %s\n%!"

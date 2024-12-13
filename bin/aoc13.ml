open Utils

(* Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400 *)
let parse_block xs =
  let get_button s =
    let regex = Str.regexp {|Button [A|B]: X\+\([0-9]+\), Y\+\([0-9]+\)|} in
    match get_all_groups regex s with
    | (_, [x; y])::[] -> (int_of_string x, int_of_string y)
    | _ -> failwith ("Invalid input: "^s)
  in
  let get_prize s =
    let regex = Str.regexp {|Prize: X=\([0-9]+\), Y=\([0-9]+\)|} in
    match get_all_groups regex s with
    | (_, [x; y])::[] -> (int_of_string x, int_of_string y)
    | _ -> failwith ("Invalid input: "^s)
  in
  let ax,ay = get_button (List.nth xs 0) in
  let bx,by = get_button (List.nth xs 1) in
  let gx,gy = get_prize (List.nth xs 2) in
  (ax,ay),(bx,by),(gx,gy)

let data = 
  "inputs/13.txt"
  |> read_lines
  |> group_by (fun x -> x = "")
  |> List.map parse_block

let find_cost (ax,ay) (bx,by) (gx,gy) =
  let a = Smt.intVar "a" in
  let b = Smt.intVar "b" in
  Smt.eval_opt Smt.[
    (a >= int 0);
    (b >= int 0);
    (* (a <= int 100);
    (b <= int 100); *)
    (a*int ax+b*int bx = int gx);
    (a*int ay+b*int by = int gy);
  ] 
  Smt.(int 3*a+b)
  (function
    | Ok m -> 
      let a = Smt.eval_int m a in
      let b = Smt.eval_int m b in
      let cost = 3*a+b in
      cost
    | Error _ -> 0
  )

let () =
  data
  |> List.map (fun (a,b,g) -> find_cost a b g)
  |> List.fold_left (+) 0
  |> Printf.printf "Part 1: %d\n%!"

let () =
  data
  |> List.map (fun (a,b,(gx,gy)) -> 
    let offset = 10000000000000 in
    (a,b,(gx+offset,gy+offset))
  )  
  |> List.map (fun (a,b,g) -> find_cost a b g)
  |> List.fold_left (+) 0
  |> Printf.printf "Part 2: %d\n%!"
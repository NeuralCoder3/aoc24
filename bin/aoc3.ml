open Utils

let data = 
  (* part 1 sample is not part 2 sample *)
  "inputs/3.txt"
  |> read_file


let muls = 
  get_all_groups
  (Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|})
  data
  |> List.map (fun (_,ms) -> 
    match ms with
    | [m1;m2] -> (int_of_string m1,int_of_string m2)
    | _ -> failwith "Invalid input"
  )

let () =
  muls
  |> List.fold_left (fun acc (x,y) -> acc + x * y) 0
  |> Printf.printf "Part1: %d\n%!"

type choice = 
  | Do
  | Dont
  | Mul of int * int

let () = 
  data 
  |> get_all_groups (Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|})
  |> List.map (fun (m,ms) -> 
    if m = "do()" then Do else
    if m = "don't()" then Dont else
    match ms with
    | [m1;m2] -> Mul (int_of_string m1,int_of_string m2)
    | _ -> failwith ("Invalid input: " ^ m)
  )
  |> List.fold_left (fun (active, res) x -> 
    match x with
    | Mul (x,y) -> if active then (active, res + x * y) else (active, res)
    | Do -> (true, res)
    | Dont -> (false, res)
    ) (true,0)
  |> snd
  |> Printf.printf "Part2: %d\n%!"
  
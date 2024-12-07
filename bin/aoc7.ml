open Utils

let data = 
  "inputs/7.txt"
  |> read_lines
  |> List.map (
    fun line -> 
      match split ":" line 
      |> List.filter (fun x -> x <> "") with
      | [x;y] -> (int_of_string x,
        y |> split " "
        |> List.filter (fun x -> x <> "")
        |> List.map int_of_string
      ) 
      | _ -> failwith "Invalid input"
  ) 

let concat x y = 
  let x = string_of_int x in
  let y = string_of_int y in
  int_of_string (x ^ y)

let test with_concat res xs =
  let rec aux xs =
    match xs with 
    | [] -> failwith "Invalid input"
    | [x] -> [x]
    | x::xr -> 
      let r = aux xr in
      List.map (fun y -> x * y) r @ 
      List.map (fun y -> x + y) r @
      (if with_concat then List.map (fun y -> concat y x) r else [])
  in
  aux (List.rev xs) |> List.exists ((=) res)

let () =
  data
  |> List.filter (fun (res,xs) -> (test false res xs))
  |> List.map fst 
  |> List.fold_left (+) 0
  |> Printf.printf "Part1: %d\n%!"

let () =
  data
  |> List.filter (fun (res,xs) -> (test true res xs))
  |> List.map fst 
  |> List.fold_left (+) 0
  |> Printf.printf "Part2: %d\n%!"
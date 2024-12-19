open Utils

let data = 
  "inputs/19.txt"
  |> read_lines

let towels = List.hd data |> split ", " |> List.map explode

let starts_with t xs = 
  take (List.length t) xs = t

let possible = 
  memo_rec (fun possible xs ->
    if xs = [] then 1 else
      List.map (fun t -> 
        if starts_with t xs then 
          possible (drop (List.length t) xs)
        else 0
      ) towels |> List.fold_left ( + ) 0
  )

let () =
  data
  |> List.tl
  |> List.tl
  |> List.map explode
  |> List.map (fun xs -> possible xs)
  |> List.filter (fun x -> x > 0)
  |> List.length
  |> Printf.printf "Part 1: %d\n%!"
  
let () =
  data
  |> List.tl
  |> List.tl
  |> List.map explode
  |> List.map (fun xs -> possible xs)
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n%!"
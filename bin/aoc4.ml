open Utils

let data = 
  "inputs/4.txt"
  |> read_lines
  |> List.filter (fun x -> x <> "")
  |> List.map explode
  |> List.map Array.of_list
  |> Array.of_list

let width = Array.length data.(0)
let height = Array.length data

let count x y =
  (* check if horizontally XMAS *)
  (if 
    x >= 3 && 
    ((data.(y).(x) = 'S' 
    && data.(y).(x-1) = 'A'
    && data.(y).(x-2) = 'M'
    && data.(y).(x-3) = 'X') ||
    (data.(y).(x) = 'X'
    && data.(y).(x-1) = 'M'
    && data.(y).(x-2) = 'A'
    && data.(y).(x-3) = 'S'))
  then 1 else 0)+
  (* check if vertically XMAS *)
  (if y >= 3 && 
    ((data.(y).(x) = 'S' 
    && data.(y-1).(x) = 'A'
    && data.(y-2).(x) = 'M'
    && data.(y-3).(x) = 'X') ||
    (data.(y).(x) = 'X'
    && data.(y-1).(x) = 'M'
    && data.(y-2).(x) = 'A'
    && data.(y-3).(x) = 'S'))
  then 1 else 0)+
  (* check if diagonally XMAS *)
  (if x >= 3 && y >= 3 && 
    ((data.(y).(x) = 'S' 
    && data.(y-1).(x-1) = 'A'
    && data.(y-2).(x-2) = 'M'
    && data.(y-3).(x-3) = 'X') ||
    (data.(y).(x) = 'X'
    && data.(y-1).(x-1) = 'M'
    && data.(y-2).(x-2) = 'A'
    && data.(y-3).(x-3) = 'S'))
  then 1 else 0)+
  (if x >= 3 && y < height - 3 && 
    ((data.(y).(x) = 'S' 
    && data.(y+1).(x-1) = 'A'
    && data.(y+2).(x-2) = 'M'
    && data.(y+3).(x-3) = 'X') ||
    (data.(y).(x) = 'X'
    && data.(y+1).(x-1) = 'M'
    && data.(y+2).(x-2) = 'A'
    && data.(y+3).(x-3) = 'S'))
  then 1 else 0)+
  0

let full_count f =
  let count = 
    Array.init height (fun y -> 
      Array.init width (fun x -> f x y)
    )
  in
  Array.fold_left (fun acc x -> acc + Array.fold_left (+) 0 x) 0 count

let () = full_count count |> Printf.printf "Part1: %d\n%!"

let count2 x y =
  (* MAS cross with MAS *)
  if 
    x < width - 2 && y < height - 2 && 
    (* left one from top to bottom, right one from top to bottom *)
    ((data.(y).(x) = 'M'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x+2) = 'S'
    && data.(y).(x+2) = 'M'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x) = 'S') ||
    (* left flipped *)
    (data.(y).(x) = 'S'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x+2) = 'M'
    && data.(y).(x+2) = 'M'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x) = 'S') ||
    (* right flipped *)
    (data.(y).(x) = 'M'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x+2) = 'S'
    && data.(y).(x+2) = 'S'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x) = 'M') ||
    (* both flipped *)
    (data.(y).(x) = 'S'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x+2) = 'M'
    && data.(y).(x+2) = 'S'
    && data.(y+1).(x+1) = 'A'
    && data.(y+2).(x) = 'M')
    )
  then 1 else 0

let () = full_count count2 |> Printf.printf "Part2: %d\n%!"
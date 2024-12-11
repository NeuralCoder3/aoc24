open Utils

let data = 
  (* "inputs/10_2.txt" *)
  "inputs/10.txt"
  |> read_lines
  |> List.map (fun s -> explode s |> List.map (fun c ->
  int_of_char c - int_of_char '0'))
  |> List.map Array.of_list
  |> Array.of_list


(* all pairs (shortest) path *)
(* path if orthogonal adjacent and node is exactly one larger *)

let width = Array.length data.(0)
let height = Array.length data

let nextPos (x,y) = 
  let v = data.(y).(x) in
  [(x,y-1);(x,y+1);(x-1,y);(x+1,y)]
  |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < width && y < height)
  |> List.filter (fun (x,y) -> data.(y).(x) = v + 1)

let rec bfs (x,y) =
  let rec aux q visited =
    match q with
    | [] -> visited
    | (x,y)::qr ->
      let next = nextPos (x,y) in
      let next = List.filter (fun p -> not (List.mem p visited)) next in
      let visited = (x,y)::visited in
      aux (qr@next) visited
  in
  aux [(x,y)] []

let paths =
  Array.mapi (fun y row ->
    Array.mapi (fun x v ->
      v, (x,y), bfs (x,y)
    ) row
  ) data
  |> Array.map Array.to_list
  |> Array.to_list
  |> List.concat
  |> List.filter (fun (v,(x,y),path) -> 
    v = 0
  )
  |> List.map (fun (v,(x,y),path) -> 
    path 
    |> List.sort_uniq compare
    |> List.map (fun (x,y) -> data.(y).(x))
    |> List.filter (fun x -> x = 9)
    |> List.length
  )
  |> List.fold_left (+) 0
  |> Printf.printf "Part 1: %d\n%!"


(* count path to a 9 *)
let rec dfs (x,y) =
  if x < 0 || y < 0 || x >= width || y >= height then
    0
  else
    let v = data.(y).(x) in
    if v = 9 then
      1
    else 
      nextPos (x,y)
      |> List.map dfs
      |> List.fold_left (+) 0

let paths =
  Array.mapi (fun y row ->
    Array.mapi (fun x v ->
      v, (x,y)
    ) row
  ) data
  |> Array.map Array.to_list
  |> Array.to_list
  |> List.concat
  |> List.filter_map (fun (v,(x,y)) -> 
    if v = 0 then Some (dfs (x,y)) else None
  )
  |> List.fold_left (+) 0
  |> Printf.printf "Part 2: %d\n%!"
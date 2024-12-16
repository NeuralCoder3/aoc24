open Utils

let board = 
  "inputs/16.txt"
  |> read_lines
  |> List.map explode
  |> List.map Array.of_list
  |> Array.of_list

let height = Array.length board
let width = Array.length board.(0)


let rec find c x y =
  if board.(y).(x) = c then x,y
  else if x+1 < width then find c (x+1) y
  else find c 0 (y+1)

let sx,sy = find 'S' 0 0
let ex,ey = find 'E' 0 0
let () =
  board.(sy).(sx) <- '.';
  board.(ey).(ex) <- '.'

type direction = Up | Down | Left | Right
let next (x,y) d =
  match d with
  | Up -> x,y-1
  | Down -> x,y+1
  | Left -> x-1,y
  | Right -> x+1,y

let rot_left = function
  | Up -> Left
  | Down -> Right
  | Left -> Down
  | Right -> Up

let rot_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let dir_index = function
  | Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3

let rec dijkstra parents q dist =
  if Queue.is_empty q then ()
  else
    let (x,y),rot = Queue.pop q in
    let d = dist.(y).(x).(dir_index rot) in
    let update x' y' rot' cost =
      if x' >= 0 && x' < width && y' >= 0 && y' < height && board.(y').(x') <> '#' then
        let d' = d+cost in
        let rot'_idx = dir_index rot' in
        if dist.(y').(x').(rot'_idx) = -1 || d' < dist.(y').(x').(rot'_idx) then
          begin
            parents.(y').(x').(rot'_idx) <- [(x,y,dir_index rot)];
            dist.(y').(x').(rot'_idx) <- d';
            Queue.push ((x',y'),rot') q
          end
        else if d' = dist.(y').(x').(rot'_idx) then
          begin
            parents.(y').(x').(rot'_idx) <- (x,y,dir_index rot)::parents.(y').(x').(rot'_idx)
          end
      else ()
    in

    let x',y' = next (x,y) rot in
    update x' y' rot 1;
    update x y (rot_left rot) 1000;
    update x y (rot_right rot) 1000;
    dijkstra parents q dist

let dist,parents = 
  let parents = Array.init height (fun _ -> Array.init width (fun _ -> Array.make 4 [])) in
  let dist = Array.init height (fun _ -> Array.init width (fun _ -> Array.make 4 (-1))) in
  dist.(sy).(sx).(dir_index Right) <- 0;
  let q = Queue.create () in
  Queue.push ((sx,sy),Right) q;
  dijkstra parents q dist;
  dist,parents

let () =
  Printf.printf "Part 1: %d\n%!" 
    (
      [Up;Down;Left;Right]
      |> List.map (fun d -> dist.(ey).(ex).(dir_index d))
      |> List.filter (fun x -> x <> -1)
      |> List.fold_left min max_int
    )

let rec collect_parents parents (x,y,rot) =
  let p = parents.(y).(x).(rot) in
  List.map (collect_parents parents) p
  |> List.concat
  |> List.cons (x,y,rot)


let () =
  Printf.printf "Part 2: %d\n%!" 
    (
      [Up;Down;Left;Right]
      |> List.map (fun d -> collect_parents parents (ex,ey,dir_index d))
      |> List.concat
      |> List.map (fun (x,y,_) -> (x,y))
      |> List.sort_uniq compare
      |> List.length
    )
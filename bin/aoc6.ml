open Utils

type point = Guard | Wall | Open
type dir = Up | Down | Left | Right

let data = 
  "inputs/6.txt"
  |> read_lines
  |> List.map explode
  |> List.map (List.map (function
    | '.' -> Open
    | '#' -> Wall
    | '^' -> Guard
    | _ -> failwith "Invalid input"
  ))

let gx,gy = 
  data
  |> List.mapi (fun i xs ->
    List.mapi (fun j x ->
      if x = Guard then
        Some (j,i)
      else
        None
    ) xs
  )
  |> List.concat
  |> List.filter_map Fun.id
  |> List.hd

let data = List.map (List.map (fun x -> if x = Guard then Open else x)) data
        |> List.map Array.of_list
        |> Array.of_list

let width = Array.length data.(0)
let height = Array.length data

let nextPos (x,y) = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)

let rotate = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let rec trace_path (gx,gy) dir =
  (gx,gy) ::
  (
    let ngx,ngy = nextPos (gx,gy) dir in
    if ngx >= width || ngy >= height || ngx < 0 || ngy < 0 then [] else
    (gx,gy)::match data.(ngy).(ngx) with
    | Wall -> trace_path (gx,gy) (rotate dir)
    | Open -> trace_path (ngx,ngy) dir
    | _ -> failwith "Impossible"
  )

let () =
  trace_path (gx,gy) Up
  |> List.sort_uniq compare
  |> List.length
  |> Printf.printf "Part 1: %d\n%!"

let rec contain_loop_aux visited (gx,gy) dir =
  let key = ((gx,gy),dir) in
  if Hashtbl.mem visited key then true else
  let ngx,ngy = nextPos (gx,gy) dir in
  if ngx >= width || ngy >= height then false else
  if ngx < 0 || ngy < 0 then false else
  (
  assert (ngx < width && ngy < height && ngx >= 0 && ngy >= 0);
  Hashtbl.add visited key ();
  match data.(ngy).(ngx) with
  | Wall -> contain_loop_aux visited (gx,gy) (rotate dir)
  | Open -> contain_loop_aux visited (ngx,ngy) dir
  | _ -> failwith "Impossible"
  )

let contain_loop () =
  contain_loop_aux (Hashtbl.create 1000) (gx,gy) Up

let () =
  List.init height (fun y -> List.init width (fun x -> (x,y)))
  |> List.flatten
  |> List.filter (fun (x,y) ->
    assert (y < height && x < width && y >= 0 && x >= 0);
    if x = gx && y = gy then false else
    match data.(y).(x) with
    | Wall -> false
    | Open -> 
      data.(y).(x) <- Wall;
      let res = contain_loop () in
      data.(y).(x) <- Open;
      res
    | _ -> failwith "Impossible"
  )
  |> List.length
  |> Printf.printf "Part 2: %d\n%!"
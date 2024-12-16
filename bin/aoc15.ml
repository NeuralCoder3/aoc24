open Utils

let board,moves = 
  match "inputs/15.txt"
  |> read_lines
  |> group_by (fun x -> x = "") with
  | [a; b] -> a, b
  | _ -> failwith "Invalid input"

let moves = String.concat "" moves
let board = List.map explode board

let rx,ry = 
  (* find the @ symbol *)
  board
  |> List.mapi (fun y xs ->
    List.mapi (fun x v ->
      if v = '@' then
        Some (x,y)
      else
        None
    ) xs
  )
  |> List.concat
  |> List.filter_map Fun.id
  |> List.hd

let board = List.map Array.of_list board |> Array.of_list
let () = board.(ry).(rx) <- '.'

let original_board = Array.map Array.copy board

let rec move (x,y) c (ox,oy) = 
  if board.(y).(x) = '.' then
    (board.(y).(x) <- c; true)
  else if board.(y).(x) = '#' then
    false
  else 
    (
      assert (board.(y).(x) = 'O');
    if move (x+ox,y+oy) board.(y).(x) (ox,oy) then
      (board.(y).(x) <- c; true)
    else
      false
  )

let show_board (rx,ry) =
  Array.iteri (fun y xs ->
    Array.iteri (fun x c ->
      if x = rx && y = ry then
        Printf.printf "@"
      else
        Printf.printf "%c" c
    ) xs;
    Printf.printf "\n"
  )

let () =
    List.fold_left (fun (x,y) c ->
      let (ox,oy) = match c with
        | '^' -> (0,-1)
        | 'v' -> (0,1)
        | '<' -> (-1,0)
        | '>' -> (1,0)
        | _ -> failwith "Invalid move"
      in
      let r = move (x+ox,y+oy) '.' (ox,oy) in
      if r then
        (x+ox,y+oy)
      else
        (x,y)
    ) (rx,ry) (explode moves)
    |> ignore;
    Array.mapi (fun y xs ->
      Array.mapi (fun x c ->
        if c = 'O' then
          (y*100+x)
        else 0
      ) xs
    ) board
    |> Array.fold_left (fun acc xs ->
      Array.fold_left (+) acc xs
    ) 0
    |> Printf.printf "Part 1: %d\n%!"


let rx = 2*rx
let ry = ry
let board = 
  Array.mapi (fun y xs ->
    Array.mapi (fun x c ->
      match c with
      | '#' | '.' -> [c;c]
      | 'O' -> ['[';']']
      | _ -> failwith "Invalid input"
    ) xs
    |> Array.to_list  
    |> List.concat
    |> Array.of_list
  ) original_board


let rec move_horizontal (x,y) c ox = 
  if board.(y).(x) = '.' then
    (
        (board.(y).(x) <- c; true)
    )
  else if board.(y).(x) = '#' then
    false
  else 
    (
      assert (board.(y).(x) = '[' || board.(y).(x) = ']');
    if move_horizontal (x+ox,y) board.(y).(x) ox then
      (board.(y).(x) <- c; true)
    else
      false
  )



let rec can_move_vertical (x,y) oy = 
  if board.(y).(x) = '.' then true
  else if board.(y).(x) = '#' then false
  else 
    (let d = board.(y).(x) in
      let nx =
        if d = '[' then
          x+1
        else if d = ']' then
          x-1
        else
          failwith "Invalid input"
      in
      let ny = y+oy in
      can_move_vertical (x,ny) oy &&
      can_move_vertical (nx,ny) oy
    )

let rec move_vertical (x,y) c oy = 
  if can_move_vertical (x,y) oy then
    let d = board.(y).(x) in
    if d = '.' then
      (board.(y).(x) <- c; true)
    else 
      let nx,nd =
        if d = '[' then
          x+1,']'
        else if d = ']' then
          x-1,'['
        else
          failwith "Invalid input"
      in
      (
      board.(y).(x) <- c;
      board.(y).(nx) <- '.';
      move_vertical (x,y+oy) d oy |> ignore;
      move_vertical (nx,y+oy) nd oy |> ignore;
      true)
  else
    false

let () =
    List.fold_left (fun (x,y) c ->
      let (ox,oy) = match c with
        | '^' -> (0,-1)
        | 'v' -> (0,1)
        | '<' -> (-1,0)
        | '>' -> (1,0)
        | _ -> failwith "Invalid move"
      in
      let r = 
        if ox = 0 then
          move_vertical (x,y+oy) '.' oy
        else
          move_horizontal (x+ox,y) '.' ox
      in
      let nx,ny = 
        if r then
          (x+ox,y+oy)
        else
          (x,y)
      in
      (
        (* show_board (nx,ny) board;
        Printf.printf "\n"; *)
        (nx,ny)
      )
    ) (rx,ry) (explode moves)
    |> ignore;
    Array.mapi (fun y xs ->
      Array.mapi (fun x c ->
        if c = '[' then
          (y*100+x)
        else 0
      ) xs
    ) board
    |> Array.fold_left (fun acc xs ->
      Array.fold_left (+) acc xs
    ) 0
    |> Printf.printf "Part 2: %d\n%!"
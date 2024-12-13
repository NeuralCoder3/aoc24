open Utils

let data = 
  "inputs/12.txt"
  |> read_lines
  |> List.map explode
  |> List.map Array.of_list
  |> Array.of_list

let nextPos = [(0,-1);(1,0);(0,1);(-1,0)]

let width = Array.length data.(0)
let height = Array.length data

let rec regions visited (x,y) c =
  if x < 0 || y < 0 || x >= width || y >= height || data.(y).(x) <> c then
    (* one perimiter no area *)
    (1,0, [])
  else
  if Hashtbl.mem visited (x,y) then (0,0,[]) else
      (
        Hashtbl.add visited (x,y) true;
      List.fold_left (fun (p,a,xs) (dx,dy) ->
        let p',a',xs' = regions visited (x+dx,y+dy) c in
        (p+p',a+a',xs'@xs)
      ) (0,1,[(x,y)]) nextPos
      )

let () =
  let visited = Hashtbl.create 100 in
  Array.mapi (fun y xs ->
    Array.mapi (fun x c ->
      if Hashtbl.mem visited (x,y) then (0,0,[]) else
      let p,n,xs = regions visited (x,y) c in
      (* Printf.printf "%c: %d,%d\n" c p n; *)
      (p,n,xs)
    ) xs
  ) data
  |> Array.to_list
  |> List.map Array.to_list
  |> List.concat
  |> List.map (fun (p,n,_) -> p*n)
  |> List.fold_left (+) 0
  |> Printf.printf "Part 1: %d\n%!"

type direction = Up | Down | Left | Right

let oob (x,y) = x < 0 || y < 0 || x >= width || y >= height

let coords dir (x,y) = match dir with
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)

let right_of = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let left_of = function
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up

(* hug the edge -- follow it until a bend *)
let rec count_sides visited (x,y) dir =
  let d (x,y) = data.(y).(x) in
  let same (x2,y2) = 
    not (oob (x2,y2)) && d (x2,y2) = d (x,y) 
  in
  let diff (x2,y2) = oob (x2,y2) || d (x2,y2) <> d (x,y) in
  let dir_coord = coords dir (x,y) in
  let right_coord = coords (right_of dir) (x,y) in
  if Hashtbl.mem visited ((x,y),dir) then 0 else
  (Hashtbl.add visited ((x,y),dir) ();
  match diff dir_coord, same right_coord with
  | true, true -> count_sides visited right_coord dir (* continue along *)
  | false, _ -> 1+count_sides visited dir_coord (left_of dir) (* turn left *)
  | true, false -> 1+count_sides visited (x,y) (right_of dir) (* turn right *)
  )

let rec find_up (x,y) c =
  let nx,ny = coords Up (x,y) in
  if oob (nx,ny) || data.(ny).(nx) <> c then (x,y) else find_up (nx,ny) c

let () =
  let visited = Hashtbl.create 100 in
  Array.mapi (fun y xs ->
    Array.mapi (fun x c ->
      if Hashtbl.mem visited (x,y) then (0,0) else
      let _,n,xs = regions visited (x,y) c in
      (* (p,n,xs) *)
      let visited2 = Hashtbl.create 100 in
      (* for each position go up until not inside anymore *)
      (* if not visited, start with up *)
      let s = List.fold_left (fun acc (x,y) ->
        let x,y = find_up (x,y) c in
        if Hashtbl.mem visited2 ((x,y),Up) then acc else
        let s = count_sides visited2 (x,y) Up in
        s+acc
      ) 0 xs in
      (s,n)
    ) xs
  ) data
  |> Array.to_list
  |> List.map Array.to_list
  |> List.concat
  |> List.map (fun (s,n) -> s*n)
  |> List.fold_left (+) 0
  |> Printf.printf "Part 2: %d\n%!"
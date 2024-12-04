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

let slice arr (ox,oy) len (sx,sy) =
  List.init len (fun i -> arr.(oy + i*sy).(ox + i*sx))

let count x y =
  let valid_slice xs =
    xs = ['X';'M';'A';'S'] || xs = ['S';'A';'M';'X']
  in
  (* horizontal *)
  (if x < width - 3 && valid_slice (slice data (x,y) 4 (1,0)) then 1 else 0) +
  (* vertical *)
  (if y < height - 3 && valid_slice (slice data (x,y) 4 (0,1)) then 1 else 0) +
  (* diagonal *)
  (if x < width - 3 && y < height - 3 && valid_slice (slice data (x,y) 4 (1,1)) then 1 else 0) +
  (if x >= 3 && y < height - 3 && valid_slice (slice data (x,y) 4 (-1,1)) then 1 else 0)


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
  let valid_slice xs = xs = ['M';'A';'S'] || xs = ['S';'A';'M'] in
  if 
    x < width - 2 && y < height - 2 && 
    valid_slice (slice data (x,y) 3 (1,1)) &&
    valid_slice (slice data (x+2,y) 3 (-1,1)) 
  then 1 else 0

let () = full_count count2 |> Printf.printf "Part2: %d\n%!"
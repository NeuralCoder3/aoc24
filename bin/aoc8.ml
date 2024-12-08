open Utils

let data = 
  "inputs/8.txt"
  |> read_lines
  |> List.map explode

let height = List.length data
let width = List.length (List.hd data)

let data =
  data
  |> List.mapi (fun y row -> List.mapi (fun x c -> ((x,y),c)) row)
  |> List.flatten
  |> List.filter (fun (_,c) -> '.' <> c)

let find_overlap (x1,y1) (x2,y2) =
      let dx = x2 - x1 in
      let dy = y2 - y1 in
      let lx,ly = x1 - dx, y1 - dy in
      let rx,ry = x2 + dx, y2 + dy in
      [(lx,ly);(rx,ry)]

let antinodes find_overlap =
  List.map (fun ((x1,y1),c1) -> 
    List.map (fun ((x2,y2),c2) -> 
      if x1 = x2 && y1 = y2 then [] else 
      if c1 <> c2 then [] else
      find_overlap (x1,y1) (x2,y2)
    ) data
  ) data
  |> List.flatten
  |> List.flatten
  |> List.filter (fun (x,y) -> x >= 0 && x < width && y >= 0 && y < height)
  |> List.sort_uniq compare
  |> List.length

let () =
  antinodes find_overlap
  |> Printf.printf "Part1: %d\n%!"


let gcd a b =
  let rec aux a b =
    if b = 0 then a else aux b (a mod b)
  in
  aux (abs a) (abs b)


let find_overlap (x1,y1) (x2,y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let g = gcd dx dy in
  let dx = dx / g in
  let dy = dy / g in
  let rec aux dx dy x y =
    (x,y) :: 
    let x2 = x + dx in
    let y2 = y + dy in
    if x2 < 0 || x2 >= width || y2 < 0 || y2 >= height then [] else
    aux dx dy x2 y2
  in
  aux dx dy x1 y1@
  aux ~-dx ~-dy x1 y1
  |> List.sort_uniq compare

let () =
  antinodes find_overlap
  |> Printf.printf "Part1: %d\n%!"
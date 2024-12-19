open Utils

let width,height = 71,71
let data = 
  "inputs/18.txt"
  |> read_lines
  |> List.map (fun s ->
    match split "," s with
    | [a;b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid input"
  )

let board n =
  let b = Array.make_matrix width height false in
  take n data
  |> List.iter (fun (x,y) -> b.(x).(y) <- true);
  b

let dijkstra b =
  let open Queue in
  let q = create () in
  let dist = Array.make_matrix width height max_int in
  let add (x,y) d =
    if x >= 0 && x < width && y >= 0 && y < height then
    if not b.(x).(y) && dist.(x).(y) > d+1 then
    begin
      dist.(x).(y) <- d+1;
      push ((x,y),d+1) q
    end
  in
  dist.(0).(0) <- 0;
  push ((0,0),0) q;
  while not (is_empty q) do
    let ((x,y),d) = take q in
    add (x-1,y) d;
    add (x+1,y) d;
    add (x,y-1) d;
    add (x,y+1) d
  done;
  dist

let () =
  let b = board 1024 in
  let dist = dijkstra b in
  let res = dist.(width-1).(height-1) in
  dump_int 1 res

let () =
  (* find the point where no longer a passage is available *)
  let rec find n =
    let b = board n in
    let dist = dijkstra b in
    let res = dist.(width-1).(height-1) in
    if res = max_int then List.nth data (n-1)
    else find (n+1)
  in
  let x,y = find 0 in
  Printf.printf "Part 2: %d,%d\n%!" x y
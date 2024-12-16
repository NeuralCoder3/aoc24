open Utils

let parse_data s =
  (* p=0,4 v=3,-3 *)
  let re = Str.regexp {|p=\([0-9-]+\),\([0-9-]+\) v=\([0-9-]+\),\([0-9-]+\)|} in
  match get_all_groups re s with
  | [(_, [px; py; vx; vy])] -> (int_of_string px, int_of_string py), (int_of_string vx, int_of_string vy)
  | _ -> failwith ("Invalid input: "^s)


(* let width,height = 11,7  *)
let width,height = 101,103
let data = 
  "inputs/14.txt"
  |> read_lines
  |> List.map parse_data

let rec flip n x = 
  if x < 0 then
    flip n (x+n)
  else
    x mod n
    

let simulate_steps n data =
  List.map (fun ((px,py),(vx,vy)) ->
    (
      flip width (px+n*vx),
      flip height (py+n*vy)
    ),(vx,vy)
  ) data

let histogram data =
  List.fold_left (fun acc (x,y) ->
    match List.assoc_opt (x,y) acc with
    | None -> ((x,y),1)::acc
    | Some n -> ((x,y),n+1)::(List.remove_assoc (x,y) acc)
  ) [] data

let is_quadrant low_x low_y (x,y) =
  (if low_x then 
    x < width/2
  else
    x > (width)/2) &&
  (if low_y then
    y < height/2
  else
    y > (height)/2)



let () =
    let coords = data
    |> simulate_steps 100
    |> List.map (fun (pos,_) -> pos)
    in
    List.map (fun (low_x,low_y) ->
        coords
        |> List.filter (fun (x,y) -> is_quadrant low_x low_y (x,y))
        |> List.length
    ) [
      (true,true);
      (true,false);
      (false,true);
      (false,false)
    ] 
    |> List.fold_left ( * ) 1
    |> Printf.printf "Part 1: %d\n%!"



let show_grid coords =
  let data = 
    coords
    |> List.sort compare
    |> histogram
  in
  List.init height (fun y ->
    List.init width (fun x ->
      match List.assoc_opt (x,y) data with
      | None -> '.'
      | Some n -> Char.chr (n+Char.code '0')
    )
  )
  |> List.map (fun xs -> String.of_seq (List.to_seq xs))
  |> String.concat "\n"
  |> print_endline


let variance coords =
  let data = coords in
  let x_mean = 
    List.fold_left (fun acc (x,_) -> acc + x) 0 data
    |> float_of_int
    |> fun x -> x /. float_of_int (List.length data)
  in
  let y_mean = 
    List.fold_left (fun acc (_,y) -> acc + y) 0 data
    |> float_of_int
    |> fun x -> x /. float_of_int (List.length data)
  in
  let x_var = 
    List.fold_left (fun acc (x,_) -> acc +. (float_of_int x -. x_mean) ** 2.0) 0.0 data
    |> fun x -> x /. float_of_int (List.length data)
  in
  let y_var = 
    List.fold_left (fun acc (_,y) -> acc +. (float_of_int y -. y_mean) ** 2.0) 0.0 data
    |> fun x -> x /. float_of_int (List.length data)
  in
  x_var+.y_var

let () = 
    let max = width*height in
    iter (fun (i,data, (max_var, max_i)) ->
      let new_data = simulate_steps 1 data in
      let coords = new_data |> List.map (fun (pos,_) -> pos) in
      let v = variance coords in
      (* Printf.printf "%d, %f\n" (i+1) variance; *)
      (* Printf.printf "\n\n\n%d:\n" (i+1);
      show_grid coords; *)
      (i+1,new_data, 
        if v < max_var then
          (v, i+1)
        else
          (max_var, max_i)
      )
    ) max (0,data,(10000.0,0))
    |> (fun (_,_,(_,i)) -> Printf.printf "Part 2: %d\n%!" i)
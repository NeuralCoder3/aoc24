open Utils

let data = 
  "inputs/5.txt"
  |> read_lines
  |> group_by (fun x -> x = "")

let orderings =
  List.nth data 0
  |> List.map (fun s ->
    match split "|" s with
    | [a; b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid input"
  )

let updates =
  List.nth data 1
  |> List.map (fun s ->
    split "," s
    |> List.map int_of_string 
  )

let hull update = 
  List.mapi (fun i x -> 
    List.mapi (fun j y -> 
      if i < j then
        Some (x, y)
      else
        None
    ) update
  ) update
  |> List.concat
  |> List.filter_map Fun.id

let correct =
  updates
  |> List.filter (fun update ->
    let h = hull update in
    let consistent = 
      not (List.exists (fun (a, b) ->
        List.exists (fun (c, d) ->
          (* a = d && b = c *)
          (a,b) = (d,c)
        ) h
      ) orderings)
    in
    consistent
  )

let update_score updates =
  updates
  |> List.map (fun xs ->
    List.nth xs (List.length xs / 2)
  )
  |> List.fold_left (+) 0

let () =
  correct
  |> update_score
  |> Printf.printf "Part 1: %d\n%!"

let wrong_updates = 
  List.filter (fun update ->
    not (List.mem update correct)
  ) updates

let () =
    let compare orderings u1 u2 =
      if List.exists (fun (a, b) -> (a,b)=(u2,u1)) orderings then 1 else
      if List.exists (fun (a, b) -> (a,b)=(u1,u2)) orderings then -1 else
        0
    in
    wrong_updates
    |> List.map (fun xs ->
      List.sort (compare orderings) xs
    )
    |> update_score
    |> Printf.printf "Part 2: %d\n%!"

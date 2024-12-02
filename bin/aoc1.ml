open Utils

let data = 
  "inputs/1_1.txt"
  |> read_lines
  |> List.map (
    fun line -> 
      match split " " line 
      |> List.filter (fun x -> x <> "")
      |> List.map int_of_string with
      | [x;y] -> (x,y) | _ -> failwith "Invalid input"
  )
let data1,data = List.split data
let () =
      let d1 = List.sort compare data1 in
      let d2 = List.sort compare data in
      List.combine d1 d2
      |> List.map (fun (x,y) -> abs (x-y))
      |> List.fold_left (+) 0
      |> Printf.printf "Part1: %d\n%!"

let histogram xs =
  List.fold_left (fun acc x -> 
    match List.assoc_opt x acc with
    | None -> (x,1)::acc
    | Some n -> (x,n+1)::(List.remove_assoc x acc)
  ) [] xs

let () =
    let h2 = histogram data in
    List.fold_left (fun acc x -> 
      let count = 
        match List.assoc_opt x h2 with
        | None -> 0
        | Some n -> n
      in acc + count * x
    ) 0 data1
    |> Printf.printf "Part2: %d\n%!"
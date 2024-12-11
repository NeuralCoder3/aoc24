open Utils

let data = 
  "inputs/11.txt"
  |> read_file
  |> split " "
  |> List.filter ((<>) "")
  |> List.map int_of_string


let mutate x =
  if x = 0 then [1] else 
    let s = string_of_int x in
    let l = String.length s in
  if l mod 2 = 0 then 
    let first_half = String.sub s 0 (l/2) in
    let second_half = String.sub s (l/2) (l/2) in
    [int_of_string first_half; int_of_string second_half]
  else
    [x*2024]

let blink xs = 
  List.map mutate xs
  |> List.concat

let () =
  data 
  |> iter blink 25
  |> List.length
  |> Printf.printf "Part 1: %d\n%!"

let histogram xs =
  List.fold_left (fun acc x ->
    match List.assoc_opt x acc with
    | None -> (x,1)::acc
    | Some n -> (x,n+1)::(List.remove_assoc x acc)
  ) [] xs

let combine xs =
  List.fold_left (fun acc (x,n) ->
    match List.assoc_opt x acc with
    | None -> (x,n)::acc
    | Some m -> (x,n+m)::(List.remove_assoc x acc)
  ) [] xs

let blink2 xs = 
  List.map (fun (x,n) -> List.map (fun x -> (x,n)) (mutate x)) xs
  |> List.concat
  |> List.sort compare
  |> combine

let () =
  data 
  |> histogram
  |> iter blink2 75
  |> List.map (fun (_,n) -> n)
  |> List.fold_left (+) 0
  |> Printf.printf "Part 2: %d\n%!"
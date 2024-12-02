open Utils

let data = 
  "inputs/2_1.txt"
  |> read_lines
  |> List.map (
    fun line -> 
      split " " line 
      |> List.filter (fun x -> x <> "")
      |> List.map int_of_string 
  )

let safe xs =
    let sorted = List.sort compare xs in
    let increasing = sorted = xs in
    let decreasing = List.rev sorted = xs in
    let safe_distance = 
      zipNext xs
      |> List.for_all (fun (x,y) -> 1 <= abs (x-y) && abs (x-y) <= 3)
    in
    (increasing || decreasing) && safe_distance

let () =
  List.filter safe data
  |> List.length
  |> Printf.printf "Part1: %d\n%!"

let () =
  data
  |> List.filter (fun xs ->
    List.init (List.length xs) (fun i -> take i xs @ drop (i+1) xs)
    |> List.exists safe
  )
  |> List.length
  |> Printf.printf "Part2: %d\n%!"
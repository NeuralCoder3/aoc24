open Utils

(* id and length *)
type blocks = File of int*int | Space of int

let data = 
  "inputs/9.txt"
  |> read_file
  |> explode
  |> List.map (fun c -> int_of_string (String.make 1 c))
  |> List.fold_left (fun (is_file, id, acc, pos) x ->
    (
      not is_file, 
      (if is_file then id+1 else id),
      (if is_file then 
        File(id, x)
      else
        Space x)::acc, 
      pos+x
    )
  ) (true, 0, [],0)
  |> fun (_,_,acc,_) -> List.rev acc


let data1 =
    let rec aux xs =
      match xs with
      | [] -> []
      | x :: xr -> 
        match !x with
        | File(id,len) -> File(id,len)::aux xr
        | Space len ->
          let rec decrease len ys =
            match ys with 
            | [] -> []
            | y :: yr ->
              match !y with
              | File(id2,len2) -> 
                if len2 > len then
                  (
                    (* Printf.printf "Move %d bytes (out of %d) from %d\n" len len2 id2; *)
                    y := File(id2, len2-len); [File(id2,len)])
                else if len2 = len then
                  (
                    (* Printf.printf "Move all %d bytes from %d\n" len id2; *)
                    y := Space len; [File(id2,len)])
                else (* len2 < len *)
                  (
                    (* Printf.printf "Move all %d bytes from %d, remaining space\n" len id2; *)
                    y := Space len2; 
                    File(id2,len2) :: decrease (len-len2) yr)
              | Space _ -> decrease len yr
          in 
          (* Printf.printf "fill Space(%d)\n" len; *)
          let new_blocks = decrease len (List.rev xr) in
          let remainder = aux xr in
          new_blocks @ remainder
    in
    aux (List.map ref data)

let print_filesystem xs =
  xs
  |> List.iter (function
    | File(id,len) -> Printf.printf "File(%d,%d)\n" id len
    | Space len -> Printf.printf "Space(%d)\n" len
  )


let score xs = 
  List.fold_left (fun (sum,pos) x ->
    match x with 
    | File(id,len) -> (sum +
      (List.init len (fun i -> (pos+i) * id) |> List.fold_left ( + ) 0)
      , pos+len)
    | Space len -> (sum, pos+len)
  ) (0,0) xs
  |> fst

let () =
  data1
  |> score
  |> Printf.printf "Part 1: %d\n%!"


let data2 =
  let rec try_insert xs =
    match xs with
    | [] -> []
    | Space len::xr ->
      Space len::try_insert xr
    | File(id,len)::xr ->

      let rec insert_space len ys =
        match ys with
        | [] -> File (id,len), []
        | Space len2::yr ->
          if len2 > len then
            (
              Space len, File (id,len)::Space (len2-len)::yr
            )
          else if len2 = len then
            (
              Space len, File (id,len)::yr
            )
          else
            (
              let file,remainder = insert_space len yr in
              file, Space len2::remainder
            )
        | File (id2,len2)::yr ->
          let file,remainder = insert_space len yr in
          file, File (id2,len2)::remainder
      in
      let element, remainder = insert_space len (List.rev xr) in
      element :: try_insert (List.rev remainder)
  in
  List.rev (try_insert (List.rev data))

let () =
  data2
  |> score
  |> Printf.printf "Part 2: %d\n%!"
let parse ss =
  let rec aux rs = function
    | [] -> assert false
    | "" :: is ->
      List.rev rs |> List.map (fun s ->
          Scanf.sscanf s "%d-%d" (fun a b -> a, b)) ,
      List.map int_of_string is
    | x :: xs ->
      aux (x :: rs) xs
  in
  aux [] ss

let between (a, b) i =
  a <= i && i <= b

let is_fresh rs i =
  List.exists (fun r -> between r i) rs

let solve1 (rs, is) =
  List.filter (is_fresh rs) is
  |> List.length

module S = struct
  let empty = []

  let rec add (a, b) = function
    | [] -> [a, b]
    | (u, v as h) :: xs ->
      if b < u then
        (a, b) :: h :: xs
      else begin
        match between h a, between h b with
        | false, false ->
          if a < u && v < b then
            (a, u-1) :: (u, v) :: add (v + 1, b) xs
          else
            h :: add (a, b) xs
        | true, false -> h :: add (v + 1, b) xs
        | false, true -> (a, u - 1) :: h :: xs
        | true, true -> (u, v) :: xs
      end

  let cardinal s =
    List.map (fun (a, b) -> b - a + 1) s
    |> List.fold_left Int.add 0
end


let solve2 (rs, _) =
  List.fold_left (fun s r -> S.add r s) S.empty rs
  |> S.cardinal

let () =
    let data = open_in "input.txt"
    |> In_channel.input_lines
    |> parse
    in
    (* Part 1 here *)
    solve1 data |> print_int;

    print_newline ();
    (* Part 2 here *)
    solve2 data |> print_int

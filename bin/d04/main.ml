let parse ss =
  List.map (fun s -> String.to_seq s |> Array.of_seq) ss
  |> Array.of_list

let get_neigh mat x y =
  let neigh =
    let ns = [ -1; 0; 1 ] in
    List.concat_map (fun n -> List.map (fun m -> n, m) ns) ns
    |> List.filter (fun (x, y) -> not (x = 0 && y = 0))
  in
  neigh
  |> List.map (fun (dx, dy) -> x + dx, y + dy)
  |> List.map (fun (x, y) ->
      try mat.(y).(x) with _ -> '.')

let is_removable mat (x, y) =
  if mat.(y).(x) = '@' then
    get_neigh mat x y
    |> List.filter (fun c -> c = '@')
    |> List.length
    |> (fun nc -> nc < 4)
  else
    false

let solve1 mat =
  mat
  |> Array.mapi (fun y arr ->
      arr
      |> Array.mapi (fun x _ -> is_removable mat (x, y))
      |> Array.to_list
      |> List.filter Fun.id
      |> List.length)
  |> Array.fold_left Int.add 0

let find_removable mat =
  mat
  |> Array.mapi (fun y arr ->
      arr
      |> Array.mapi (fun x _ ->
          if is_removable mat (x, y) then
            Some (x, y)
          else
            None)
      |> Array.to_list
      |> List.filter_map Fun.id)
  |> Array.to_list
  |> List.concat

let solve2 mat =
  let rec aux cnt =
    let rs = find_removable mat in
    if List.is_empty rs then
      cnt
    else begin
      List.iter (fun (x, y) -> mat.(y).(x) <- 'x') rs;
      aux (cnt + List.length rs)
    end
  in
  aux 0

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

let parse str =
  String.split_on_char ',' str
  |> List.map (fun s ->
      Scanf.sscanf s "%d-%d" (fun a b -> a, b))

let halve str =
  let l = String.length str in
  if l mod 2 = 0 then
    Some (String.sub str 0 (l/2), String.sub str (l/2) (l/2))
  else
    None

let solve1 rs =
  let find_invaild_id (n, m) =
    Seq.init (m - n + 1) (fun i -> i + n)
    |> Seq.filter (fun i ->
        match halve @@ string_of_int i with
        | None -> false
        | Some (x, y) -> x = y)
    |> List.of_seq
  in
  List.concat_map find_invaild_id rs
  |> List.fold_left Int.add 0

let is_invalid2 n =
  let n = string_of_int n in
  let l = String.length n in
  let aux i =
    if l mod i = 0 then
      Seq.init (l/i) (fun j -> j*i)
      |> Seq.map (fun j -> String.sub n j i)
      |> Seq.uncons
      |> Option.fold ~none: false ~some: (fun (h, t) ->
          Seq.for_all (fun x -> x = h) t)
    else false
  in
  Seq.init (l/2) (fun i -> i + 1)
  |> Seq.exists aux

let solve2 rs =
  let find_invaild_id (n, m) =
    Seq.init (m - n + 1) (fun i -> i + n)
    |> Seq.filter is_invalid2
    |> List.of_seq
  in
  List.concat_map find_invaild_id rs
  |> List.fold_left Int.add 0

let () =
  let data = open_in "input.txt"
    |> In_channel.input_all
    |> parse
  in
  (* Part 1 here *)
  solve1 data |> print_int;

  print_newline ();
  (* Part 2 here *)
  solve2 data |> print_int

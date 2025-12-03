let parse str =
  String.to_seq str
  |> Seq.map (fun c -> Char.(code c - code '0'))
  |> List.of_seq

let max_joltage1 ns =
  let rec combi = function
    | [] -> []
    | n :: ns ->
      List.map (fun m -> n * 10 + m) ns @ combi ns
  in
  combi ns
  |> List.fold_left max min_int

let max_joltage2 ns =
  (* find leftmost biggest digit and partition at its position *)
  let partition_by_max ns =
    List.fold_left (fun (l, m, r) n ->
        if n > m then (* new max *)
          l @ [m] @ r, n, []
        else
          l, m, r @ [n])
      ([], List.hd ns, [])
      (List.tl ns)
  in
  (* greedy way of finding the maximum. *)
  let rec aux i ns =
    if i = 0 then []
    else
      let rec loop (l, m, r) =
        if List.length r >= i-1 then (* has enough digits *)
          m :: aux (i-1) r
        else (* try using left digit of it *)
          let ll, lm, lr = partition_by_max l in
          loop (ll, lm, lr @ [m] @ r)
      in
      loop @@ partition_by_max ns
  in
  aux 12 ns
  |> List.fold_left (fun acc n -> acc * 10 + n) 0

let solve f data =
  List.map f data
  |> List.fold_left Int.add 0

let () =
    let data = open_in "input.txt"
    |> In_channel.input_lines
    |> List.map parse
    in
    (* Part 1 here *)
    solve max_joltage1 data |> print_int;
    print_newline ();
    (* Part 2 here *)
    solve max_joltage2 data |> print_int

let solve1 data =
    List.fold_left (fun (d, c) (r, n) ->
        let d = (d + (if r = 'L' then -1 else 1) * n) mod 100 in
        d, (if d = 0 then c + 1 else c))
    (50, 0)
    data
    |> snd

let solve2 data =
    let rec aux d c s n =
        if n = 0 then d, c
        else
            let d = (d + s + 100) mod 100 in
            let c = c + if d = 0 then 1 else 0 in
            aux d c s (n - 1)
    in
    List.fold_left (fun (d, c) (r, n) ->
        let s = if r = 'L' then -1 else 1 in
        aux d c s n)
    (50, 0)
    data
    |> snd

let parse str =
    Scanf.sscanf str "%c%d" (fun c d -> c, d)

let () =
    let data = open_in "input.txt" |> In_channel.input_lines |> List.map parse in
    solve1 data |> print_int;
    print_newline ();
    solve2 data |> print_int

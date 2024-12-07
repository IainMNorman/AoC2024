module D01

let solve (lines: string[]) =

    let left, right =
        lines
        |> Array.map (fun line ->
            line
            |> fun s -> s.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> (fun parts -> (int parts[0], int parts[1])))
        |> Array.unzip

    let part1 =
        Array.map2 (fun a b -> abs (a - b)) (Array.sort left) (Array.sort right)
        |> Array.sum

    let part2 =
        left
        |> Array.sumBy (fun x -> (Array.filter (fun y -> y = x) right |> Array.length) * x)

    part1, part2

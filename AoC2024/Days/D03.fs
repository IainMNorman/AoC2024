module D03

open System.Text.RegularExpressions

let solve (input: string) =
    let part1 =
        Regex.Matches(input, @"mul\((\d{1,3}),\s*(\d{1,3})\)")
        |> Seq.cast<Match>
        |> Seq.sumBy (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)

    let part2 =
        Regex.Matches(input, @"mul\((\d{1,3}),\s*(\d{1,3})\)|do\(\)|don't\(\)")
        |> Seq.cast<Match>
        |> Seq.fold
            (fun (acc, enabled) m ->
                match (m.Value, enabled) with
                | v, true when v.StartsWith("mul") -> (acc + (int m.Groups[1].Value * int m.Groups[2].Value), enabled)
                | "do()", _ -> (acc, true)
                | "don't()", _ -> (acc, false)
                | _ -> (acc, enabled))
            (0, true)
        |> fst


    part1, part2

module D05

open System
open System.Collections.Generic

let parseInput (input: string) =
    let sections = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    (sections[0].Split('\n') |> Array.map (fun l -> let p = l.Split('|') in int p[0], int p[1]),
     sections[1].Split('\n') |> Array.map (fun l -> l.Split(',') |> Array.map int))

let sortUpdate (update: int array) (rules: (int * int) array) =
    update |> Array.sortWith (fun x y -> if Array.exists ((=) (x, y)) rules then -1 else 1)

let findMiddlePage update =
    Array.item ((Array.length update) / 2) update

let calculateSumOfMiddlePages input =
    let rules, updates = parseInput input
    updates
    |> Array.filter (fun u -> sortUpdate u rules = u)
    |> Array.map findMiddlePage
    |> Array.sum

let calculateSumOfCorrectedMiddlePages input =
    let rules, updates = parseInput input
    updates
    |> Array.filter (fun u -> sortUpdate u rules <> u)
    |> Array.map (fun u -> sortUpdate u rules)
    |> Array.map findMiddlePage
    |> Array.sum

let solve input =
    calculateSumOfMiddlePages input, calculateSumOfCorrectedMiddlePages input

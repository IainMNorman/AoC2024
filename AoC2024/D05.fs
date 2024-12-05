module D05

open System
open System.Collections.Generic

let parseInput (input: string) =
    let sections = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    (sections[0].Split('\n') |> Array.map (fun l -> let p = l.Split('|') in int p[0], int p[1]),
     sections[1].Split('\n') |> Array.map (fun l -> l.Split(',') |> Array.map int))

let isValidUpdate rules update =
    rules |> Array.forall (fun (x, y) ->
        match Array.tryFindIndex ((=) x) update, Array.tryFindIndex ((=) y) update with
        | Some xi, Some yi -> xi < yi
        | _ -> true)

let fixUpdate (update: int array) (rules: (int * int) array) =
    update |> Array.sortWith (fun x y -> if Array.exists ((=) (x, y)) rules then -1 else 1)

let findMiddlePage update =
    Array.item ((Array.length update) / 2) update

let calculateSumOfMiddlePages input =
    let rules, updates = parseInput input
    updates
    |> Array.filter (isValidUpdate rules)
    |> Array.map findMiddlePage
    |> Array.sum

let calculateSumOfCorrectedMiddlePages input =
    let rules, updates = parseInput input
    updates
    |> Array.filter (not << isValidUpdate rules)
    |> Array.map (fun u -> fixUpdate u rules)
    |> Array.map findMiddlePage
    |> Array.sum

let solve input =
    calculateSumOfMiddlePages input, calculateSumOfCorrectedMiddlePages input

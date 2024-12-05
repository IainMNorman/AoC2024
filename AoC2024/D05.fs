module D05

open System
open System.Collections.Generic

let parseInput (input: string) =
    let sections = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)

    let rules =
        sections[0].Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = line.Split('|')
            (int parts[0], int parts[1]))

    let updates =
        sections[1].Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.Split(',', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

    (rules, updates)

let isValidUpdate rules update =
    rules
    |> Array.forall (fun (x, y) ->
        if Array.contains x update && Array.contains y update then
            let xIndex = Array.findIndex ((=) x) update
            let yIndex = Array.findIndex ((=) y) update
            xIndex < yIndex
        else
            true)

let fixUpdate (update: int array) (rules: (int * int) array) =
    update |> Array.sortWith (fun n1 n2 -> if Array.exists ((=) (n1, n2)) rules then 1 else -1)

let findMiddlePage update =
    let midIndex = Array.length update / 2
    Array.item midIndex update

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

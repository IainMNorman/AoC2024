module D02

let isSafe levels =
    let differences = Array.pairwise levels |> Array.map (fun (a, b) -> b - a)
    let allIncreasing = Array.forall (fun d -> d > 0 && d <= 3) differences
    let allDecreasing = Array.forall (fun d -> d < 0 && d >= -3) differences
    allIncreasing || allDecreasing

let isSafeWithDampener levels =
    if isSafe levels then
        true
    else
        levels
        |> Array.mapi (fun i _ -> Array.append levels[.. i - 1] levels[i + 1 ..])
        |> Array.exists isSafe

let processAndFilterLines safeFunc (line: string) =
    line.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> safeFunc

let countFilteredReports lines safeFunc =
    lines
    |> Array.map (processAndFilterLines safeFunc)
    |> Array.filter id
    |> Array.length

let solve (lines: string array) =
    let part1 = countFilteredReports lines isSafe
    let part2 = countFilteredReports lines isSafeWithDampener
    part1, part2

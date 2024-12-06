module D06

open System.Collections.Generic

let readMap (puzzleInput: string) =
    puzzleInput.Trim().Split '\n'
    |> Array.map (fun line -> line.ToCharArray() |> Array.toList)
    |> Array.toList

let turnRight currentDirection =
    match currentDirection with
    | '^' -> '>'
    | '>' -> 'v'
    | 'v' -> '<'
    | '<' -> '^'
    | _ -> failwith "Invalid direction"

let moveForward (x, y) direction =
    match direction with
    | '^' -> (x - 1, y)
    | 'v' -> (x + 1, y)
    | '<' -> (x, y - 1)
    | '>' -> (x, y + 1)
    | _ -> failwith "Invalid direction"

let part1 (labMap: char list list) obsR obsC gp gd =
    let mutable looping = false
    let visitedPositions = HashSet<int * int * char>()
    let mutable guardPosition = gp
    let mutable guardDirection = gd
    let rows = labMap.Length
    let cols = labMap[0].Length

    let mutable continuePatrol = true
    while continuePatrol do
        let r, c = guardPosition

        if visitedPositions.Contains(r, c, guardDirection) then
            looping <- true
            continuePatrol <- false

        visitedPositions.Add(r, c, guardDirection) |> ignore

        // Check next position
        let nextPosition = moveForward guardPosition guardDirection
        let nextR, nextC = nextPosition

        if nextR < 0 || nextR >= rows || nextC < 0 || nextC >= cols then
            continuePatrol <- false
        else if labMap[nextR].[nextC] <> '#' && (nextR, nextC) <> (obsR, obsC) then
            // Move forward if no obstacle
            guardPosition <- nextPosition
        else
            // Turn right if obstacle or out of bounds
            guardDirection <- turnRight guardDirection

    visitedPositions.Count, looping

let part2 (labMap: char list list) p1 gp gd =
    let rows = labMap.Length
    let cols = labMap[0].Length
    let mutable loopCount = 0

    for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
            if labMap[r].[c] = '.' then
                let result = part1 labMap r c gp gd

                if snd result then
                    loopCount <- loopCount + 1

    loopCount

let solve input =
    let labMap = readMap input
    let rows = labMap.Length
    let cols = labMap[0].Length
    let mutable guardPosition = (0, 0)
    let mutable guardDirection = ' '

    for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
            match labMap[r][c] with
            | '^'
            | 'v'
            | '<'
            | '>' ->
                guardPosition <- (r, c)
                guardDirection <- labMap[r][c]
                
            | _ -> ()

    let p1 = part1 labMap -1 -1 guardPosition guardDirection
    //let p2 = part2 labMap p1 guardPosition guardDirection
    fst p1, 0

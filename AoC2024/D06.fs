module D06

open System.Collections.Generic

let readMap (puzzleInput: string) =
    puzzleInput.Trim().Split '\n'
    |> Array.map (fun line -> line.ToCharArray())

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

let part1 (labMap: char array array) obsR obsC gp gd =
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

    visitedPositions, looping

let part2 (labMap: char array array) gp gd vps =
    vps
    |> Array.filter (fun vp -> snd (part1 labMap (fst vp) (snd vp) gp gd))
    |> Array.length

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

    let p1 =
        fst (part1 labMap -1 -1 guardPosition guardDirection)
        |> Seq.map (fun (x, y, _) -> (x, y))
        |> Seq.distinct
        |> Seq.toArray
    
    let p2 = part2 labMap guardPosition guardDirection p1

    p1.Length, p2

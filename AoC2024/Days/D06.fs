module D06

open System.Collections.Generic

let readMap (puzzleInput: string) =
    puzzleInput.Trim().Split '\n' |> Array.map (fun line -> line.ToCharArray())

let turnRight currentDirection =
    (currentDirection + 1) % 4

let moveForward (x, y) direction =
    match direction with
    | 0 -> (x - 1, y)
    | 2 -> (x + 1, y)
    | 3 -> (x, y - 1)
    | 1 -> (x, y + 1)
    | _ -> failwith "Invalid direction"

let part1 (labMap: char array array) obsR obsC gp gd =
    let mutable looping = false

    let mutable guardPosition = gp
    let mutable guardDirection = gd
    let rows = labMap.Length
    let cols = labMap[0].Length

    let visitedPositions = HashSet<int * int>()

    let mutable continuePatrol = true

    while continuePatrol do
        let r, c = guardPosition

        visitedPositions.Add(r, c) |> ignore

        let nextPosition = moveForward guardPosition guardDirection
        let nextR, nextC = nextPosition

        if nextR < 0 || nextR >= rows || nextC < 0 || nextC >= cols then
            continuePatrol <- false
        else if labMap[nextR].[nextC] <> '#' && (nextR, nextC) <> (obsR, obsC) then
            guardPosition <- nextPosition
        else
            guardDirection <- turnRight guardDirection

    visitedPositions, looping

let checkForLoop (labMap: char array array) obsR obsC gp gd rows cols =
    let mutable looping = false
    let mutable guardPosition = gp
    let mutable guardDirection = gd
    let visitedPositions = Array3D.zeroCreate<bool> rows cols 4
    let mutable continuePatrol = true

    while continuePatrol do
        let r, c = guardPosition
            
        visitedPositions[r, c, guardDirection] <- true
        let nextPosition = moveForward guardPosition guardDirection
        let nextR, nextC = nextPosition
        if nextR < 0 || nextR >= rows || nextC < 0 || nextC >= cols then
            continuePatrol <- false
        else if labMap[nextR].[nextC] <> '#' && (nextR, nextC) <> (obsR, obsC) then
            guardPosition <- nextPosition
        else
            if visitedPositions[r, c, guardDirection] then
                looping <- true
                continuePatrol <- false
            guardDirection <- turnRight guardDirection
            
    looping

let part2 (labMap: char array array) gp gd vps =
    let rows = labMap.Length
    let cols = labMap[0].Length

    vps
    |> Array.filter (fun vp -> checkForLoop labMap (fst vp) (snd vp) gp gd rows cols)
    |> Array.length

let solve input =
    let labMap = readMap input
    let rows = labMap.Length
    let cols = labMap[0].Length
    let mutable guardPosition = (0, 0)
    let mutable guardDirection = -1

    for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
            match labMap[r][c] with
            | '^' ->
                guardPosition <- (r, c)
                guardDirection <- 0
            | 'v' ->
                guardPosition <- (r, c)
                guardDirection <- 2
            | '<' ->
                guardPosition <- (r, c)
                guardDirection <- 3
            | '>' ->
                guardPosition <- (r, c)
                guardDirection <- 1

            | _ -> ()

    let p1 =
        fst (part1 labMap -1 -1 guardPosition guardDirection) |> Seq.toArray

    let p2 = part2 labMap guardPosition guardDirection p1

    p1.Length, p2

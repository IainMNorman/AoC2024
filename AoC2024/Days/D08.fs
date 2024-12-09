module D08

let stringToBytesASCII (input: string) =
    System.Text.Encoding.ASCII.GetBytes(input)

let parseAntennas lines =
    lines
    |> Array.mapi (fun row line ->
        stringToBytesASCII line
        |> Array.mapi (fun col value ->
            match value with
            | 46uy -> None
            | _ -> Some((row, col), value)))
    |> Array.concat
    |> Array.choose id
    |> Map.ofArray // immutable map of input without periods

let getVector ((ax, ay), (bx, by)) = (bx - ax, by - ay)

let addVector (x, y) (dx, dy) = (x + dx, y + dy)

let subVector (x, y) (dx, dy) = (x - dx, y - dy)

let inBounds w h (x, y) = x >= 0 && x < w && y >= 0 && y < h

let pairToAntiNodePair pair =
    let vector = getVector pair
    (subVector (fst pair) vector, addVector (snd pair) vector)

let pairToAllAntiNodes w h pair =
    let vector = getVector pair

    let collectPoints startPoint stepFn =
        List.unfold (fun p ->
            if inBounds w h p then Some(p, stepFn p) else None
        ) startPoint

    collectPoints (fst pair) (fun p -> subVector p vector) @
    collectPoints (snd pair) (fun p -> addVector p vector)
    
let part1 lines =
    parseAntennas lines
    |> Seq.groupBy _.Value
    |> Seq.map (fun (_, group) ->
        let keys = group |> Seq.map _.Key |> Seq.toList

        keys
        |> Seq.collect (fun i -> keys |> List.skipWhile ((<>) i) |> List.tail |> Seq.map (fun j -> (i, j))))
    |> Seq.concat
    |> Seq.map pairToAntiNodePair
    |> Seq.collect (fun (a, b) -> [ a; b ])
    |> Seq.distinct
    |> Seq.filter (inBounds lines[0].Length lines.Length)
    |> Seq.length
    
let part2 lines =
    parseAntennas lines
    |> Seq.groupBy _.Value
    |> Seq.map (fun (_, group) ->
        let keys = group |> Seq.map _.Key |> Seq.toList
    
        keys
        |> Seq.collect (fun i -> keys |> List.skipWhile ((<>) i) |> List.tail |> Seq.map (fun j -> (i, j))))
    |> Seq.concat
    |> Seq.map (fun p -> pairToAllAntiNodes lines[0].Length lines.Length p )
    |> Seq.concat
    |> Seq.distinct
    |> Seq.filter (inBounds lines[0].Length lines.Length)
    |> Seq.length

let solve lines = part1 lines, part2 lines

let test =
    part1 (
        """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""
            .Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    )

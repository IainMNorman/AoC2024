module D09

let parseInput (input: string) =
    input
    |> Seq.map (fun c -> c |> string |> int)
    |> Seq.chunkBySize 2
    |> Seq.mapi (fun i chunk ->
        match chunk with
        | [|f;s|] -> Seq.append
                         (Seq.replicate f i)
                         (Seq.replicate s -1)
        | [|f|] -> Seq.replicate f i
        | _ -> Seq.empty) 
    |> Seq.concat
    |> Seq.toArray
    
let checksum (disk: int array) =
    disk
    |> Array.mapi (fun i c ->
        match c with
        | -1 -> 0
        | _ -> (c |> string |> int) * i)
    |> Array.map int64
    |> Array.sum
    
let part1 input =
    let disk = parseInput input
    let spaceCount = disk |> Array.filter (fun i -> i = -1) |> Array.length
    for i in 1 .. spaceCount do
        let indexOfNextSpace = disk |> Array.findIndex (fun x -> x = -1)
        disk[indexOfNextSpace] <- disk[disk.Length - i]
        disk[disk.Length - i] <- -1
    checksum disk

let solve input = part1 input, 0

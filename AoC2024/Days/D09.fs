module D09

let parseInput (input: string) =
    input
    |> Seq.map (fun c -> c |> string |> int)
    |> Seq.chunkBySize 2
    |> Seq.mapi (fun i chunk ->
        match chunk with
        | [| f; s |] -> Seq.append (Seq.replicate f i) (Seq.replicate s -1)
        | [| f |] -> Seq.replicate f i
        | _ -> Seq.empty)
    |> Seq.concat
    |> Seq.toArray

let checksum (disk: int array) =
    disk
    |> Array.map int64
    |> Array.mapi (fun i c ->
        match c with
        | -1L -> 0L
        | _ -> (c * (int64 i))
        )
    |> Array.sum

let getIndexAndCount (fileId: int) (disk: int array) =
    let firstIndex = disk |> Array.findIndex (fun id -> id = fileId)
    let count = disk |> Array.fold (fun acc id -> if id = fileId then acc + 1 else acc) 0
    (firstIndex, count)

let findContiguousSpaceToLeft currentStart size (disk: int array) =
    let rec findIndex start count =
        if start + size > currentStart + size then
            None
        elif count = size then
            Some(start - size)
        elif disk[start] = -1 then
            findIndex (start + 1) (count + 1)
        else
            findIndex (start + 1) 0

    findIndex 0 0

let part1 input =
    let disk = parseInput input
    let spaceCount = disk |> Array.filter (fun i -> i = -1) |> Array.length

    for i in 1..spaceCount do
        let indexOfNextSpace = disk |> Array.findIndex (fun x -> x = -1)
        disk[indexOfNextSpace] <- disk[disk.Length - i]
        disk[disk.Length - i] <- -1

    checksum disk

let part2 input =
    let disk = parseInput input

    let descendingValues = seq { (disk |> Array.max) .. -1 .. 0 }
    descendingValues
    |> Seq.iter (fun v ->
        let sourceRange = getIndexAndCount v disk
        let space = findContiguousSpaceToLeft (fst sourceRange) (snd sourceRange) disk
        if space.IsSome then
            Array.fill disk (fst sourceRange) (snd sourceRange) -1
            Array.fill disk space.Value (snd sourceRange) v)
    
    disk
    |> checksum
        

let solve input =
    //let test = part2 "2333133121414131499"
    part1 input, part2 input

// part1 "2333133121414131402"
// part2 "2333133121414131499" 
// parseInput "2333133121414131402"

//6398097037768
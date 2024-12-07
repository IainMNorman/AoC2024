module D07

let parseLine (line: string) =
    line
    |> _.Split([| ": " |], System.StringSplitOptions.None)
    |> fun parts ->
        parts[0] |> int64,
        parts[1]
        |> fun (s: string) -> s.Split([| ' ' |], System.StringSplitOptions.None) |> Array.map int64

let parseLines (lines: string array) = lines |> Array.map parseLine

let generateOperatorCombinations count opSymbols =
    printfn "Getting combinations"
    let rec combine acc depth =
        if depth = 0 then acc
        else 
            combine 
                (Array.collect (fun x -> Array.map (fun y -> Array.append [| y |] x) opSymbols) acc) 
                (depth - 1)
    combine (Array.singleton [||]) count

let getOperatorCombinationsCache (lines: (int64 * int64 array) array) (opSymbols: string array) =
    printfn $"Getting cache for {opSymbols}"
    let minOps = lines |> Array.minBy (fun l -> Array.length (snd l)) |> snd |> Array.length
    let maxOps = lines |> Array.maxBy (fun l -> Array.length (snd l)) |> snd |> Array.length
    let range = seq { minOps..maxOps } |> Seq.toArray

    range
    |> Array.map (fun count ->
        let ops = generateOperatorCombinations (count - 1) opSymbols
        count, ops)
    |> Map.ofArray

let concatNumbers (a: int64) (b: int64) =
    let rec countDigits (n: int64) =
        if n < 10 then 1 else 1 + countDigits (n / int64 10)

    let bDigits = countDigits b
    a * (pown (int64 10) bDigits) + b

let calculate (numbers: int64 array) (ops: string array) =
    Array.fold2
        (fun acc op num ->
            match op with
            | "+" -> acc + num
            | "*" -> acc * num
            | "||" -> concatNumbers acc num
            | _ -> failwith "Unexpected operator")
        numbers[0] // Initial accumulator is the first number
        ops
        numbers[1..] // Skip the first number for folding

let canReachTestValue testValue (numbers: int64 array) opCombinationsCache =
    Map.find (Array.length numbers) opCombinationsCache
    |> Array.exists (fun ops -> calculate numbers ops = testValue)

let part1 lines =
    let parsedLines = parseLines lines
    let opCombinationsCache = getOperatorCombinationsCache parsedLines [|"+";"*"|]

    parsedLines
    |> Array.filter (fun (testValue, numbers) -> canReachTestValue testValue numbers opCombinationsCache)
    |> Array.sumBy fst


let part2 lines =
    let parsedLines = parseLines lines
    let opCombinationsCache = getOperatorCombinationsCache parsedLines [|"+";"*";"||"|]

    parsedLines
    |> Array.filter (fun (testValue, numbers) -> canReachTestValue testValue numbers opCombinationsCache)
    |> Array.sumBy fst

let solve lines = part1 lines, part2 lines

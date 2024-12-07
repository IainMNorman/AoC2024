module D04

let charSequenceToString (chars: seq<char>) : string = System.String.Concat(chars)

let columns (lines: string array) =
    lines
    |> Array.map Seq.toArray
    |> Array.transpose
    |> Array.map (fun x -> x |> charSequenceToString)

let diagonals (lines: string array) =
    let rows = lines.Length
    let cols = lines[0].Length

    let getDiagonal (startRow, startCol) (rowStep, colStep) =
        Seq.unfold
            (fun (row, col) ->
                if row >= 0 && row < rows && col >= 0 && col < cols then
                    Some(lines[row][col], (row + rowStep, col + colStep))
                else
                    None)
            (startRow, startCol)
        |> Seq.toArray
        |> charSequenceToString

    let d1 =
        Array.init rows (fun row -> getDiagonal (row, 0) (1, 1))
        |> Array.filter (fun diag -> diag.Length >= 4)

    let d2 =
        Array.init (cols - 1) (fun i -> getDiagonal (0, i + 1) (1, 1))
        |> Array.filter (fun diag -> diag.Length >= 4)

    let d3 =
        Array.init rows (fun row -> getDiagonal (row, cols - 1) (1, -1))
        |> Array.filter (fun diag -> diag.Length >= 4)

    let d4 =
        Array.init (cols - 1) (fun i -> getDiagonal (0, i) (1, -1))
        |> Array.filter (fun diag -> diag.Length >= 4)

    Array.concat [ d1; d2; d3; d4 ]

let getAllStrings lines =
    Array.concat [ lines; columns lines; diagonals lines ]

let count (fourLetterTerm: string) (str: string) =
    str
    |> Seq.windowed 4
    |> Seq.map (fun window -> System.String(window))
    |> Seq.filter (fun s -> s = fourLetterTerm)
    |> Seq.length

let findXMas grid =
    let getChar (r, c) =
        if r >= 0 && c >= 0 && r < Array.length grid && c < String.length grid[0] then
            Some grid[r].[c]
        else
            None

    let isXMas (r, c) =
        let topLeft = getChar (r - 1, c - 1)
        let topRight = getChar (r - 1, c + 1)
        let bottomLeft = getChar (r + 1, c - 1)
        let bottomRight = getChar (r + 1, c + 1)

        let diagonal1 = (topLeft, bottomRight)
        let diagonal2 = (topRight, bottomLeft)

        match (diagonal1, diagonal2) with
        | (Some 'M', Some 'S'), (Some 'M', Some 'S') -> true
        | (Some 'S', Some 'M'), (Some 'S', Some 'M') -> true
        | (Some 'M', Some 'S'), (Some 'S', Some 'M') -> true
        | (Some 'S', Some 'M'), (Some 'M', Some 'S') -> true
        | _ -> false

    grid
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c ch -> if ch = 'A' && isXMas (r, c) then 1 else 0)
        |> Seq.sum)
    |> Array.sum

let solve (lines: string array) =
    let part1 =
        let allStrings = getAllStrings lines

        (allStrings |> Array.sumBy (count "XMAS"))
        + (allStrings |> Array.sumBy (count "SAMX"))

    part1, findXMas lines

﻿// For more information see https://aka.ms/fsharp-console-apps

// let d01 = D01.solve (System.IO.File.ReadAllLines("i01.txt"))
// printfn $"Day 01 : Part 1: %d{fst d01} -- Part 2: %d{snd d01}"

// let d02 = D02.solve (System.IO.File.ReadAllLines("i02.txt"))
// printfn $"Day 02 : Part 1: %d{fst d02} -- Part 2: %d{snd d02}"

// let d03 = D03.solve (System.IO.File.ReadAllText("i03.txt"))
// printfn $"Day 03 : Part 1: %d{fst d03} -- Part 2: %d{snd d03}"

// let d04 = D04.solve (System.IO.File.ReadAllLines("i04.txt"))
// printfn $"Day 04 : Part 1: %d{fst d04} -- Part 2: %d{snd d04}"

// let d05 = D05.solve (System.IO.File.ReadAllText("i05.txt"))
// printfn $"Day 05 : Part 1: %d{fst d05} -- Part 2: %d{snd d05}"

// let d06 = D06.solve (System.IO.File.ReadAllText("i06.txt"))
// printfn $"Day 06 : Part 1: %d{fst d06} -- Part 2: %d{snd d06}"

let test = D06.solve "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"

printfn $"Test : Part 1: %d{fst test} -- Part 2: %d{snd test}"


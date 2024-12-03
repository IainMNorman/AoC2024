// For more information see https://aka.ms/fsharp-console-apps

let d01 = D01.solve (System.IO.File.ReadAllLines("i01.txt"))
printfn $"Day 01 : Part 1: %d{fst d01} -- Part 2: %d{snd d01}"

let d02 = D02.solve (System.IO.File.ReadAllLines("i02.txt"))
printfn $"Day 02 : Part 1: %d{fst d02} -- Part 2: %d{snd d02}"

let d03 = D03.solve (System.IO.File.ReadAllText("i03.txt"))
printfn $"Day 03 : Part 1: %d{fst d03} -- Part 2: %d{snd d03}"





// let d02 = D03.solve (("7 6 4 2 1
// 1 2 7 8 9
// 9 7 6 2 1
// 1 3 2 4 5
// 8 6 4 4 1
// 1 3 6 7 9").Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries))
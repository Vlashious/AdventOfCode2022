open System.IO

type Range = seq<int>
type Pair = Range * Range

let arrayToTuple (array: int array) =
    match array with
    | [| first; second |] -> (first, second)
    | _ -> failwith "Error"

let tupleToSeq tuple =
    let (f, s) = fst tuple, snd tuple
    seq { f..s }

let pairFullyContainsOneOfRanges pair =
    let (range1, range2) = (Set.ofSeq <| fst pair, Set.ofSeq <| snd pair)
    let intersection = Set.intersect range1 range2
    let check = intersection.Count > 0
    check

let readStream =
    File.ReadAllLinesAsync "input.txt" |> Async.AwaitTask |> Async.RunSynchronously

let pairStream =
    seq {
        for line in readStream do
            let input = line.Split ","

            let getRange (inp: string[]) (index: int) =
                (inp[ index ].Split "-") |> Array.map (fun x -> int x)

            let rangeConverter index =
                getRange input index |> arrayToTuple |> tupleToSeq

            let firstRange = rangeConverter 0
            let secondRange = rangeConverter 1
            yield Pair(firstRange, secondRange)
    }

let solve =
    let total = 0

    (total, pairStream)
    ||> Seq.fold (fun acc x -> if pairFullyContainsOneOfRanges x then acc + 1 else acc)

printfn $"{solve}"

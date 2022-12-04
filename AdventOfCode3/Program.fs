open System.IO
open System

type Rucksack = string

let toCharSet (str: string) = str.ToCharArray() |> Set.ofArray

let findSameItemsInChunks (rucksacks: Rucksack[]) =
    let initialSet = rucksacks[0] |> toCharSet
    (initialSet, rucksacks)
    ||> Array.fold (fun acc x -> Set.intersect acc (toCharSet x))

let smallItems = seq { 'a' .. 'z' }
let bigItems = seq { 'A' .. 'Z' }

let mapPriority item =
    let isSmallItem = smallItems |> Seq.contains item
    let isBigItem = bigItems |> Seq.contains item

    match isSmallItem, isBigItem with
    | true, false -> Convert.ToInt32 item - 96
    | false, true -> Convert.ToInt32 item - 38
    | _ -> failwith "Impossible."

let readSequence =
    File.ReadAllLinesAsync "input.txt"
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> Array.toSeq

let rucksackSequence =
    seq {
        for line in readSequence do
        yield Rucksack line
    }

let rucksackByThreeSequence = Seq.chunkBySize 3 rucksackSequence

let repeatedItemsSequence =
    seq {
        for rucksacks in rucksackByThreeSequence do
            let groupBadge = findSameItemsInChunks rucksacks
            yield groupBadge
    }

let prioritySequence =
    seq {
        for repeatedItems in repeatedItemsSequence do
            yield repeatedItems |> Set.map (fun x -> mapPriority x)
    }

let totalPriority =
    let mutable total = 0

    for priority in prioritySequence do
        total <- (+) total (priority |> Set.toSeq |> Seq.sum)

    total

printfn $"{totalPriority}"

// smallItems |> Seq.iter (fun x -> printfn $"{mapPriority x}")
// bigItems |> Seq.iter (fun x -> printfn $"{mapPriority x}")

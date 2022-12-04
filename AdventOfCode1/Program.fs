open System.IO

type Action = Accumulate of int | Next

let readTask = seq {
    let allLines = File.ReadAllLinesAsync "input.txt" |> Async.AwaitTask |> Async.RunSynchronously
    for line in allLines do
        yield line
}

let inputStream = seq {
    for line in readTask do
        match line with
        | "" -> Next
        | value -> Accumulate(int value)
}

let calorieStream = seq {
    let mutable temp = 0
    for action in inputStream do
        match action with
        | Accumulate v -> temp <- temp + v
        | Next ->
            yield temp
            temp <- 0
}

let topThreeCalories = calorieStream 
                                    |> Seq.sortDescending
                                    |> Seq.take 3

let maxCalories = topThreeCalories |> Seq.sum

let answer = maxCalories
printf $"{answer}"
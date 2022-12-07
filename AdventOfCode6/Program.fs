open System.IO

type IndexedChar = int * char

let readStream =
    File.ReadAllTextAsync "input.txt"
    |> Async.AwaitTask
    |> Async.RunSynchronously

let characterStream =
    seq {
        let allCharacters = readStream.ToCharArray() |> Array.indexed
        let mutable index = 0

        while index < allCharacters.Length do
            yield
                allCharacters
                |> Array.skip index
                |> Array.truncate 14 // 4 for first part

            index <- index + 1
    }

let checkTask =
    let unrepeatedSequence =
        characterStream
        |> Seq.find (fun x ->
            let symbols = x |> Array.map (fun x -> snd x)
            let distinctSymbols = symbols |> Array.distinct
            let isUnrepeated = symbols.Length = distinctSymbols.Length
            isUnrepeated)

    let indexes = unrepeatedSequence |> Array.map (fun x -> fst x)
    indexes |> Array.last

printfn $"{checkTask + 1}"

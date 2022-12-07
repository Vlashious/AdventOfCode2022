open System.IO

type Crate = char
type Stack = { Index: int; Crates: Crate list }
type Arrangement = Stack list
type Move = { Amount: int; From: int; To: int }

type ParseAction =
    | PushStack of string
    | FinishStacks of string
    | Move of string

let getStackInArrangement (arrangement: Arrangement) (index: int) =
    arrangement
    |> List.find (fun x -> x.Index = index)

let getCratesToMove stack amount =
    stack.Crates |> List.rev |> List.take amount
//|> List.rev - first version of the problem

let applyMoveToFromStack (fromStack: Stack) amountMoved =
    let newCrates =
        fromStack.Crates
        |> List.take (fromStack.Crates.Length - amountMoved)

    { Index = fromStack.Index
      Crates = newCrates }

let applyMoveToToStack (toStack: Stack) movedCrates =
    let newCrates = movedCrates |> List.rev |> (@) toStack.Crates

    { Index = toStack.Index
      Crates = newCrates }

let readSequence =
    File.ReadAllLinesAsync "input.txt"
    |> Async.AwaitTask
    |> Async.RunSynchronously

let parseActionSequence =
    seq {
        let filteredLines = readSequence |> Array.filter (fun x -> x <> "")

        for line in filteredLines do
            match line with
            | txt when line.Contains "move" -> Move txt
            | txt when line.Contains "[" -> PushStack txt
            | txt when line.StartsWith " 1" -> FinishStacks txt
            | _ -> failwith "Impossible"
    }

let buildArrangement =
    seq {
        let mutable stacks = []

        for action in parseActionSequence do
            match action with
            | PushStack str -> stacks <- stacks @ [ str ]
            | FinishStacks str ->
                let stackIndeces = str.Split " " |> Array.filter (fun x -> x <> "")

                let indecesInString =
                    stackIndeces
                    |> Array.map (fun x -> str.IndexOf x)
                    |> Array.indexed

                for stackIndex, indexInString in indecesInString do
                    let crates =
                        stacks
                        |> List.map (fun x -> x[indexInString])
                        |> List.filter (fun x -> x <> ' ')
                        |> List.rev

                    yield
                        { Index = stackIndex + 1
                          Crates = crates }
            | _ -> ignore

    }

let buildMoves =
    ([], parseActionSequence)
    ||> Seq.fold (fun acc x ->
        match x with
        | Move str ->
            let tokens = str.Split " "
            let amount = int tokens[1]
            let fromIndex = int tokens[3]
            let toIndex = int tokens[5]

            List.append
                [ { Amount = amount
                    From = fromIndex
                    To = toIndex } ]
                acc
        | _ -> acc)

let applyMoveToArrangement arrangement move =
    let getStack = getStackInArrangement arrangement
    let fromStack = getStack move.From
    let toStack = getStack move.To
    let movedCrates = getCratesToMove fromStack move.Amount
    let newFromStack = applyMoveToFromStack fromStack move.Amount
    let newToStack = applyMoveToToStack toStack movedCrates

    arrangement
    |> List.except [ fromStack; toStack ]
    |> List.append [ newFromStack; newToStack ]

let arrangement: Arrangement = buildArrangement |> List.ofSeq
let moves = buildMoves |> List.rev

let newArrangement =
    (arrangement, moves)
    ||> List.fold (fun acc x -> applyMoveToArrangement acc x)
    |> List.sortBy (fun x -> x.Index)

printfn "%A" newArrangement

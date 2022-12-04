open System.IO

type Move =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Win
    | Lose
    | Draw

type Round = Move * Move

let movePointMap move =
    match move with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let chooseCorrectStrategy round =
    match round with
    | Rock, Win
    | Paper, Draw
    | Scissors, Lose -> Paper
    | Rock, Draw
    | Scissors, Win
    | Paper, Lose -> Rock
    | Rock, Lose
    | Scissors, Draw
    | Paper, Win -> Scissors

let moveMap inputChar =
    match inputChar with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "Unreachable code executed."

let roundResultMap inputChar =
    match inputChar with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "Unreachable code executed."

let resultPointMap result =
    match result with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let findRoundResult round =
    match round with
    | Rock, Paper
    | Paper, Scissors
    | Scissors, Rock -> Win
    | Rock, Rock
    | Paper, Paper
    | Scissors, Scissors -> Draw
    | _ -> Lose

let calculateRoundPoints round =
    let movePoints = movePointMap (snd round)
    let resultPoints = findRoundResult round |> resultPointMap
    movePoints + resultPoints

let readTask =
    File.ReadAllLinesAsync "input.txt" |> Async.AwaitTask |> Async.RunSynchronously

let roundStream =
    seq {
        for line in readTask do
            let values = line.Split " "
            let opponentMove = moveMap values[0]
            let myMove = chooseCorrectStrategy (opponentMove, roundResultMap values[1])
            yield Round <| (opponentMove, myMove)
    }

let calculateTask =
    let mutable score = 0

    for round in roundStream do
        let roundScore = calculateRoundPoints round
        score <- score + roundScore

    score

let result = calculateTask
printfn $"{result}"

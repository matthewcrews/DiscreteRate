﻿open System

open DiscreteRate

[<EntryPoint>]
let main argv =


    //let source = Tank.create "Source" infinity
    //let op1 = Operation.create "Operation1" 1.0 10.0
    //let op2 = Operation.create "Operation2" 1.0 5.0
    //let merge = Merge.create "Merge1"
    //let split = Split.create "Split1"
    //let tank1 = Tank.create "Tank1" 10.0
    //let sink = Tank.create "Sink" infinity

    let operationCount = 10

    let operations =
        [1 .. operationCount - 1]
        |> List.map (fun idx -> idx, Transform.create $"Operation{idx}" 1.0 10.0)

    let tanks =
        [0 .. operationCount - 1]
        |> List.map (fun idx -> idx, Tank.create $"Tank{idx}" 10.0)
        |> Map

    let sourceTank = tanks.[0]

    let arcs =
        operations
        |> List.collect (fun (idx, operation) -> 
            let inArc = arc.connect (tanks.[idx - 1], operation)
            let outArc = arc.connect (operation, tanks.[idx])
            [inArc; outArc]
        )

    let network =
        Network arcs

    let tankLevels =
        tanks
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (fun tank -> tank, BufferLevel.Finite 0.0)
        |> Map
        |> fun x -> x.Add (sourceTank, BufferLevel.Infinite)


    let initialState = { TankLevels = tankLevels }

    let nonMemoizeSettings = {
        MaxSolveTime_ms = 10_000L
        Memoize = false
    }

    let nonMemoizedSolver = Solver nonMemoizeSettings


    let sln = nonMemoizedSolver.Solve network initialState

    //printfn "%A" sln.FlowRates
    //printfn "%A" sln.FillRates

    0 // return an integer exit code
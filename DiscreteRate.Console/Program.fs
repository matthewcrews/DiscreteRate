open System

open DiscreteRate.Solver

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
        |> List.map (fun idx -> idx, Transform.create $"Transform{idx}" 1.0 10.0)

    let tanks =
        [0 .. operationCount - 1]
        |> List.map (fun idx -> idx, Buffer.create $"Buffer{idx}")
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
        |> Seq.map (fun tank -> tank, BufferState.Empty)
        |> Map
        |> fun x -> x.Add (sourceTank, BufferState.Partial)


    let initialState = { BufferStates = tankLevels }

    let nonMemoizeSettings = {
        MaxSolveTime_ms = 10_000L
        Memoize = false
    }

    let nonMemoizedSolver = Solver nonMemoizeSettings

    let sln = nonMemoizedSolver.Solve network initialState

    0 // return an integer exit code
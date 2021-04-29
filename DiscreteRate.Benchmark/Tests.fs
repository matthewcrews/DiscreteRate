module DiscreteRate.Benchmark


open DiscreteRate.Solver
open BenchmarkDotNet.Attributes

let operationCount = 10

//let operations =
//    [1 .. operationCount - 1]
//    |> List.map (fun idx -> idx, Transform.create $"Operation{idx}" 1.0 10.0)

//let tanks =
//    [0 .. operationCount - 1]
//    |> List.map (fun idx -> idx, Buffer.create $"Tank{idx}")
//    |> Map

//let sourceTank = tanks.[0]

//let arcs =
//    operations
//    |> List.collect (fun (idx, operation) -> 
//        let inArc = arc.connect (tanks.[idx - 1], operation)
//        let outArc = arc.connect (operation, tanks.[idx])
//        [inArc; outArc]
//    )

//let network =
//    Network arcs

//let bufferStates =
//    tanks
//    |> Map.toSeq
//    |> Seq.map snd
//    |> Seq.map (fun tank -> tank, BufferState.Empty)
//    |> Map
//    |> fun x -> x.Add (sourceTank, BufferState.Partial)


//let initialState = { BufferStates = bufferStates }


//let nonMemoizeSettings = {
//    MaxSolveTime_ms = 10_000L
//    Memoize = false
//}

//let nonMemoizedSolver = Solver nonMemoizeSettings

//let memoizedSettings = {
//    MaxSolveTime_ms = 10_000L
//    Memoize = true
//}

//let memoizedSolver = Solver memoizedSettings

//let maxIterations = 1_000

//type Benchmarks () =

//    [<Benchmark>]
//    member this.NonMemoized () =
//        let mutable testValue = 0.0
            
//        for _ in 0 .. maxIterations do
//            let sln = nonMemoizedSolver.Solve network initialState
//            testValue <- sln.FillRates.[sourceTank].Value

//        testValue

//    [<Benchmark>]
//    member this.Memoized () =
//        let mutable testValue = 0.0

//        for _ in 0 .. maxIterations do
//            let sln = memoizedSolver.Solve network initialState
//            testValue <- sln.FillRates.[sourceTank].Value

//        testValue
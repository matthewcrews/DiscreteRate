module DiscreteRate.Benchmark


open DiscreteRate
open BenchmarkDotNet.Attributes


let source = Tank.create "Source" infinity
let op1 = Operation.create "Operation1" 1.0 10.0
let op2 = Operation.create "Operation2" 1.0 5.0
let merge = Merge.create "Merge1"
let split = Split.create "Split1"
let tank1 = Tank.create "Tank1" 10.0
let sink = Tank.create "Sink" infinity

let network =
    Network [
        arc.connect (source, split)
        arc.connect (split, op1, 2.0)
        arc.connect (split, op2, 1.0)
        arc.connect (op1, sink)
        arc.connect (op2, sink)
    ]

let initialLevels = 
    [
        source, TankLevel.Infinite
        sink, TankLevel.Finite 0.0
        //tank1, TankLevel.Finite 10.0
    ] |> Map

let initialState = { TankLevels = initialLevels }

let nonMemoizeSettings = {
    MaxSolveTime_ms = 10_000L
    Memoize = false
}

let nonMemoizedSolver = Solver nonMemoizeSettings

let memoizedSettings = {
    MaxSolveTime_ms = 10_000L
    Memoize = false
}

let memoizedSolver = Solver memoizedSettings


type Benchmarks () =

    [<Benchmark>]
    member this.NonMemoized () =
        let sln = nonMemoizedSolver.Solve network initialState
        if sln.FillRates.[source].Value > 1_000_000.0 then
            failwith "What?"

    [<Benchmark>]
    member this.Memoized () =
        let sln = memoizedSolver.Solve network initialState
        if sln.FillRates.[source].Value > 1_000_000.0 then
            failwith "What?"
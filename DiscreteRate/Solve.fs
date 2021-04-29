[<AutoOpen>]
module DiscreteRate.Solve

open Flips
open Flips.Types

let createOperationConstraints (flowDecs: Map<Arc, Decision>) (transform: Transform, (inputArc, outputArc)) =

    let balanceConstraint =
        Constraint.create $"{transform.Label}_Balance" (transform.ConversionFactor.Value * flowDecs.[inputArc] == flowDecs.[outputArc])

    match transform.MaxOutputRate with
    | MaxOutputRate.Finite mf ->
        let maxFlowConstraint =
            Constraint.create $"{transform.Label}_MaxFlow" (flowDecs.[outputArc] <== mf.Value)
        [balanceConstraint; maxFlowConstraint]
    | MaxOutputRate.Infinite ->
        [balanceConstraint]


let createTankConstraints (state: NetworkState) (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Buffer, Decision>) (buffer: Buffer, (inputArcs, outputArcs)) =

    let inputExpr =
        inputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let outputExpr =
        outputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let accumulationDec = fillDecs.[buffer]

    let balanceConstraint =
        Constraint.create $"{buffer.Label}_Balance" (accumulationDec == inputExpr - outputExpr)
        |> Some

    let minAccumulationRate =
        if state.TankLevels.[buffer] <= BufferLevel.Zero && (not outputArcs.IsEmpty) then
            Constraint.create $"{buffer.Label}_MustFill" (accumulationDec >== 0.0)
            |> Some
        else
            None

    let maxAccumulationRate =
        match buffer.Capacity with
        | BufferLevel.Finite _ ->
            if state.TankLevels.[buffer] >= buffer.Capacity && (not inputArcs.IsEmpty) then
                Constraint.create $"{buffer.Label}_MustDrain" (accumulationDec <== 0.0)
                |> Some
            else
                None
        | _ -> None
            
    [balanceConstraint; minAccumulationRate; maxAccumulationRate]
    |> List.choose id


let createMergeConstraints (flowDecs: Map<Arc, Decision>) (merge: Merge, (inputArcs, outputArc)) =
    
    let inputExpr =
        inputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let balanceConstraint =
        Constraint.create $"{merge.Label}_Balance" (inputExpr == flowDecs.[outputArc])

    let proportionTotal = inputArcs |> List.sumBy (fun arc -> arc.Proportion.Value)

    let proportionalConstraints =
        ConstraintBuilder "Proportional" {
            for arc in inputArcs ->
                flowDecs.[arc] == (arc.Proportion.Value / proportionTotal) * flowDecs.[outputArc]
        } |> List.ofSeq

    balanceConstraint::proportionalConstraints


let createSplitConstraints (flowDecs: Map<Arc, Decision>) (split: Split, (inputArc, outputArcs)) =
    
    let outputExpr =
        outputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let balanceConstraint =
        Constraint.create $"{split.Label}_Balance" (flowDecs.[inputArc] == outputExpr)

    let proportionTotal = outputArcs |> List.sumBy (fun arc -> arc.Proportion.Value)

    let proportionalConstraints =
        ConstraintBuilder "Proportion" {
            for arc in outputArcs ->
                (arc.Proportion.Value / proportionTotal) * flowDecs.[inputArc] == flowDecs.[arc]
        } |> List.ofSeq

    balanceConstraint::proportionalConstraints


let buildModel (network: Network) (state: NetworkState) (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Buffer, Decision>) =

    let operationConstraints =
        network.Operation
        |> Map.toSeq
        |> Seq.collect (createOperationConstraints flowDecs)

    let tankConstraints =
        network.Tank
        |> Map.toSeq
        |> Seq.collect (createTankConstraints state flowDecs fillDecs)

    let mergeConstraints =
        network.Merge
        |> Map.toSeq
        |> Seq.collect (createMergeConstraints flowDecs)

    let splitConstraints =
        network.Split
        |> Map.toSeq
        |> Seq.collect (createSplitConstraints flowDecs)

    let flowExpr =
        network.Arcs
        |> Seq.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let maxFlowObjective =
        Objective.create "MaxFlow" Maximize flowExpr

    let model =
        Model.create maxFlowObjective
        |> Model.addConstraints operationConstraints
        |> Model.addConstraints tankConstraints
        |> Model.addConstraints mergeConstraints
        |> Model.addConstraints splitConstraints

    model

let composeResult (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Buffer, Decision>) (solution: Solution) =

    let flowRates =
        Solution.getValues solution flowDecs
        |> Map.map (fun _ flowRate -> FlowRate.create flowRate)

    let fillRates =
        Solution.getValues solution fillDecs
        |> Map.map (fun _ fillRate -> FillRate.create fillRate)

    NetworkSolution.create flowRates fillRates


let solve (solverSettings: SolverSettings) (network: Network) (state: NetworkState) =

    let flowDecs =
        DecisionBuilder "Flow" {
            for arc in network.Arcs ->
                Continuous (0.0, infinity)
        } |> Map

    let fillDecs =
        DecisionBuilder "Fill" {
            for tank in network.Tank |> Map.toSeq |> Seq.map fst ->
                Continuous (-infinity, infinity)
        } |> Map

    let model = buildModel network state flowDecs fillDecs
    let result = Solver.solve solverSettings model

    match result with
    | Optimal solution -> composeResult flowDecs fillDecs solution
    | Unbounded _ -> invalidArg (nameof network) $"Network has infinite flow capacity"
    | _ -> invalidArg (nameof network) $"Network could not be solved"


type Solver (settings: Settings) =

    let solverSettings = { Settings.basic with MaxDuration = settings.MaxSolveTime_ms; SolverType = SolverType.GLOP }
    let previousSolutions = System.Collections.Generic.Dictionary ()

    member _.Solve (network: Network) (state: NetworkState) =
        
        if settings.Memoize then
            let key = (network, state)
            match previousSolutions.TryGetValue key with
            | true, solution -> solution
            | false, _ ->
                let solution = solve solverSettings network state
                previousSolutions.Add (key, solution)
                solution
        else
            solve solverSettings network state
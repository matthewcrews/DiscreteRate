[<AutoOpen>]
module DiscreteRate.Solve

open Flips
open Flips.Types

let createOperationConstraints (flowDecs: Map<Arc, Decision>) (operation: Operation, (inputArc, outputArc)) =

    let balanceConstraint =
        Constraint.create $"{operation.Label}_Balance" (operation.ConversionFactor.Value * flowDecs.[inputArc] == flowDecs.[outputArc])

    match operation.MaxOutputRate with
    | MaxOutputRate.Finite mf ->
        let maxFlowConstraint =
            Constraint.create $"{operation.Label}_MaxFlow" (flowDecs.[outputArc] <== mf.Value)
        [balanceConstraint; maxFlowConstraint]
    | MaxOutputRate.Infinite ->
        [balanceConstraint]


let createTankConstraints (state: NetworkState) (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Tank, Decision>) (tank: Tank, (inputArcs, outputArcs)) =

    let inputExpr =
        inputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let outputExpr =
        outputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let accumulationDec = fillDecs.[tank]

    let balanceConstraint =
        Constraint.create $"{tank.Label}_Balance" (accumulationDec == inputExpr - outputExpr)
        |> Some

    let minAccumulationRate =
        if state.TankLevels.[tank] <= TankLevel.Zero && (not outputArcs.IsEmpty) then
            Constraint.create $"{tank.Label}_MustFill" (accumulationDec >== 0.0)
            |> Some
        else
            None

    let maxAccumulationRate =
        match tank.MaxLevel with
        | TankLevel.Finite _ ->
            if state.TankLevels.[tank] >= tank.MaxLevel && (not inputArcs.IsEmpty) then
                Constraint.create $"{tank.Label}_MustDrain" (accumulationDec <== 0.0)
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

    let proportionalConstraints =
        inputArcs
        |> List.pairwise 
        |> List.map (fun (arcA, arcB) ->
            Constraint.create $"{merge.Label}_{arcA.Label}_{arcB.Label}_Proportion" (arcA.Proportion.Value * flowDecs.[arcA] == arcB.Proportion.Value * flowDecs.[arcB])
        )

    balanceConstraint::proportionalConstraints


let createSplitConstraints (flowDecs: Map<Arc, Decision>) (split: Split, (inputArc, outputArcs)) =
    
    let outputExpr =
        outputArcs
        |> List.sumBy (fun arc -> 1.0 * flowDecs.[arc])

    let balanceConstraint =
        Constraint.create $"{split.Label}_Balance" (flowDecs.[inputArc] == outputExpr)

    let proportionalConstraints =
        outputArcs
        |> List.pairwise 
        |> List.map (fun (arcA, arcB) ->
            Constraint.create $"{split.Label}_{arcA.Label}_{arcB.Label}_Proportion" (arcA.Proportion.Value * flowDecs.[arcA] == arcB.Proportion.Value * flowDecs.[arcB])
        )

    balanceConstraint::proportionalConstraints


let buildModel (network: Network) (state: NetworkState) (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Tank, Decision>) =

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

let composeResult (flowDecs: Map<Arc, Decision>) (fillDecs: Map<Tank, Decision>) (solution: Solution) =

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

    let solverSettings = { Settings.basic with MaxDuration = settings.MaxSolveTime_ms; WriteLPFile = Some "test_2.lp"; SolverType = SolverType.GLOP }
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
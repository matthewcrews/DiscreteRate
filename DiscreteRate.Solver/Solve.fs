namespace DiscreteRate.Solver

open Flips
open Flips.Types


type Solver (settings: Settings) =

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

        match state.BufferStates.[buffer] with
        | BufferState.Empty ->
            let fillConstraint = Constraint.create $"{buffer.Label}_NoDrain" (accumulationDec >== 0.0)
            [balanceConstraint; fillConstraint]
        | BufferState.Full ->
            let fillConstraint = Constraint.create $"{buffer.Label}_NoFill" (accumulationDec <== 0.0)
            [balanceConstraint; fillConstraint]
        | BufferState.Partial ->
            [balanceConstraint]



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


    let solverSettings = 
        { Settings.basic with 
            MaxDuration = settings.MaxSolveTime_ms
            SolverType = SolverType.GLOP
            WriteLPFile = settings.WriteLPFile
        }

    member _.Solve (network: Network) (state: NetworkState) =
        solve solverSettings network state
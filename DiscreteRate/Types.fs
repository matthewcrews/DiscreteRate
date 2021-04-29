[<AutoOpen>]
module DiscreteRate.Types


type ConversionFactor = ConversionFactor of float
    with member this.Value =
            let (ConversionFactor value) = this
            value

type Label = Label of string
    with member this.Value = 
            let (Label value) = this
            value

type Proportion = Proportion of float
    with member this.Value = 
                let (Proportion value) = this
                value

type FlowRate = FlowRate of float
    with member this.Value =
            let (FlowRate value) = this
            value

type FillRate = FillRate of float
    with member this.Value =
            let (FillRate value) = this
            value

[<RequireQualifiedAccess>]
type TankLevel = 
    | Infinite // This must be the first case for comparison to work
    | Finite of float
    static member Zero =
        Finite 0.0

[<RequireQualifiedAccess>]
type MaxOutputRate =
    | Infinite // This must be the first case for comparison to work
    | Finite of FlowRate

type Operation = {
    Label : Label
    ConversionFactor : ConversionFactor
    MaxOutputRate : MaxOutputRate
}

//[<RequireQualifiedAccess>]
//type Capacity =
//    | Infinite // This must be the first case for comparison to work
//    | Finite of TankLevel

type Tank = {
    Label : Label
    MaxLevel : TankLevel
}

type Merge = {
    Label : Label
}

type Split = {
    Label : Label
}

[<RequireQualifiedAccess>]
type Node =
    | Operation of Operation
    | Tank of Tank
    | Merge of Merge
    | Split of Split
    with
        member this.Label =
            match this with
            | Operation n -> n.Label
            | Tank    n -> n.Label
            | Merge   n -> n.Label
            | Split   n -> n.Label

type Arc = {
    Source : Node
    Sink : Node
    Proportion : Proportion
} with
    member this.Label =
        $"{this.Source.Label}->{this.Sink.Label}"


type Network (arcs: Arc list) =

    let nodes =
        arcs
        |> List.collect (fun x -> [x.Sink; x.Source])
        |> Set

    let outboundArcs =
        arcs
        |> List.groupBy (fun x -> x.Source)
        |> Map

    let inboundArcs =
        arcs
        |> List.groupBy (fun x -> x.Sink)
        |> Map

    let (operation, tank, merge, split) =
        nodes
        |> Seq.fold (fun (operationArcs, tankArcs, mergeArcs, splitArcs) node ->
            match node with
            | Node.Operation operation ->
                let inbound =
                    match inboundArcs.[node] with
                    | [arc] -> arc
                    | _ -> invalidArg "Inbound" "Cannot have Operation with more than one input"
                let outbound =
                    match outboundArcs.[node] with
                    | [arc] -> arc
                    | _ -> invalidArg "Inbound" "Cannot have Operation with more than one output"
                let newOperationArcs = Map.add operation (inbound, outbound) operationArcs
                newOperationArcs, tankArcs, mergeArcs, splitArcs
            | Node.Tank tank ->
                let inbound = inboundArcs.[node]
                let outbound = outboundArcs.[node]
                let newTankArcs = Map.add tank (inbound, outbound) tankArcs
                operationArcs, newTankArcs, mergeArcs, splitArcs
            | Node.Merge merge ->
                let inbound = inboundArcs.[node]
                let outbound =
                    match outboundArcs.[node] with
                    | [arc] -> arc
                    | _ -> invalidArg "Inbound" "Cannot have Merge with more than one output"
                let newMergeArcs = Map.add merge (inbound, outbound) mergeArcs
                operationArcs, tankArcs, newMergeArcs, splitArcs
            | Node.Split split ->
                let inbound =
                    match inboundArcs.[node] with
                    | [arc] -> arc
                    | _ -> invalidArg "Inbound" "Cannot have Split with more than one input"
                let outbound = outboundArcs.[node]
                let newSplitArcs = Map.add split (inbound, outbound) splitArcs
                operationArcs, tankArcs, mergeArcs, newSplitArcs
        ) (Map.empty, Map.empty, Map.empty<Merge, (Arc list * Arc)>, Map.empty)

    member _.Arcs = Set arcs
    member _.Nodes = nodes
    member _.Operation = operation
    member _.Tank = tank
    member _.Merge = merge
    member _.Split = split

    override _.GetHashCode () =
        hash (operation, tank, merge, split)

    override this.Equals (b) =
        match b with
        | :? Network as other -> 
            this.Operation = other.Operation &&
            this.Tank = other.Tank &&
            this.Merge = other.Merge &&
            this.Split = other.Split
        | _ -> false


type NetworkState = {
    TankLevels : Map<Tank, TankLevel>
}

type NetworkSolution = {
    FlowRates : Map<Arc, FlowRate>
    FillRates : Map<Tank, FillRate>
}

type Settings = {
    MaxSolveTime_ms : int64
    Memoize : bool
}
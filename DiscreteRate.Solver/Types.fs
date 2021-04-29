[<AutoOpen>]
module DiscreteRate.Solver.Types


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
type BufferState =
    | Empty
    | Full
    | Partial

[<RequireQualifiedAccess>]
type MaxOutputRate =
    | Finite of FlowRate
    | Infinite // This must be the first case for comparison to work

type Transform = {
    Label : Label
    ConversionFactor : ConversionFactor
    MaxOutputRate : MaxOutputRate
}with
   override this.ToString () =
       $"Operation_{this.Label.Value}"

type Buffer = {
    Label : Label
} with
    override this.ToString () =
        $"Tank_{this.Label.Value}"

type Merge = {
    Label : Label
} with
   override this.ToString () =
       $"Merge_{this.Label.Value}"

type Split = {
    Label : Label
} with
   override this.ToString () =
       $"Split_{this.Label.Value}"

[<RequireQualifiedAccess>]
type Node =
    | Transform of Transform
    | Buffer of Buffer
    | Merge of Merge
    | Split of Split
    with
        member this.Label =
            match this with
            | Transform n -> n.Label
            | Buffer    n -> n.Label
            | Merge     n -> n.Label
            | Split     n -> n.Label

type Arc = {
    Source : Node
    Sink : Node
    Proportion : Proportion
} with
    member this.Label =
        $"{this.Source.Label}_to_{this.Sink.Label}"

    override this.ToString () =
        $"{this.Source.Label.Value}_to_{this.Sink.Label.Value}"


type Network (arcs: seq<Arc>) =

    let arcs = List.ofSeq arcs

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
            | Node.Transform operation ->
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
            | Node.Buffer tank ->
                let inbound = 
                    match Map.tryFind node inboundArcs with
                    | Some arcs -> arcs
                    | None -> []
                let outbound = 
                    match Map.tryFind node outboundArcs with
                    | Some arcs -> arcs
                    | None -> []
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
    BufferStates : Map<Buffer, BufferState>
}

type NetworkSolution = {
    FlowRates : Map<Arc, FlowRate>
    FillRates : Map<Buffer, FillRate>
}

type Settings = {
    MaxSolveTime_ms : int64
    Memoize : bool
}
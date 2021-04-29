[<AutoOpen>]
module DiscreteRate.Domain



[<RequireQualifiedAccess>]
module Label =

    let create label =
        if System.String.IsNullOrEmpty label then
            invalidArg (nameof label) $"Cannot have a null or empty Label"

        Label label


[<RequireQualifiedAccess>]
module FlowRate =

    let create flowRate =
        if flowRate < 0.0 then
            invalidArg (nameof flowRate) $"Cannot have a FlowRate less than or equal to 0.0"

        if flowRate = infinity then
            invalidArg (nameof flowRate) $"Cannot have a FlowRate of infinity"

        FlowRate flowRate


[<RequireQualifiedAccess>]
module FillRate =

    let create fillRate =
        if fillRate = -infinity then
            invalidArg (nameof fillRate) $"Cannot have a FillRate negative infinity"

        if fillRate = infinity then
            invalidArg (nameof fillRate) $"Cannot have a FillRate of infinity"

        FillRate fillRate


[<RequireQualifiedAccess>]
module MaxOutputRate =

    let create maxOutputRate =
        if maxOutputRate <= 0.0 then
            invalidArg (nameof maxOutputRate) $"Cannot have a MaxOutputRate less then or equal to 0.0"

        if maxOutputRate = infinity then
            MaxOutputRate.Infinite
        else
            MaxOutputRate.Finite (FlowRate.create maxOutputRate)


[<RequireQualifiedAccess>]
module ConversionFactor =

    let create conversionFactor =
        if conversionFactor <= 0.0 then
            invalidArg (nameof conversionFactor) $"Cannot have a ConversionFactor less than or equal to 0.0"

        if conversionFactor = infinity then
            invalidArg (nameof conversionFactor) $"Cannot have a ConversionFactor of infinity"

        ConversionFactor conversionFactor


[<RequireQualifiedAccess>]
module Proportion =

    let create proportion =
        if proportion <= 0.0 then
            invalidArg (nameof proportion) $"Cannot have a Proportion <= 0.0"

        if proportion = infinity then
            invalidArg (nameof proportion) $"Cannot have a Proportion of infinity"

        Proportion proportion


[<RequireQualifiedAccess>]
module Transform =

    let create label conversionFactor maxOutputRate =
        if conversionFactor <= 0.0 then
            invalidArg (nameof conversionFactor) $"Cannot have a {nameof conversionFactor} <= 0.0"

        {
            Label = Label.create label
            ConversionFactor = ConversionFactor conversionFactor
            MaxOutputRate = MaxOutputRate.create maxOutputRate
        }


[<RequireQualifiedAccess>]
module Merge =

    let create label : Merge =
        {
            Label = Label.create label
        }


[<RequireQualifiedAccess>]
module Split =

    let create label : Split =
        {
            Label = Label.create label
        }


[<RequireQualifiedAccess>]
module Buffer =

    let create label : Buffer =
        {
            Label = Label.create label
        }


[<RequireQualifiedAccess>]
module NetworkSolution =

    let create flowRates fillRates =
        {
            FlowRates = flowRates
            FillRates = fillRates
        }


[<RequireQualifiedAccess>]
module Node =

    let ofTransform transform =
        Node.Transform transform

    let ofBuffer buffer =
        Node.Buffer buffer

    let ofMerge merge =
        Node.Merge merge

    let ofSplit split =
        Node.Split split

    let isProcess (n: Node) =
        match n with
        | Node.Transform _ -> true
        | _ -> false


[<RequireQualifiedAccess>]
module Arc =

    let create source sink proportion =
        {
            Source = source
            Sink = sink
            Proportion = Proportion.create proportion
        }


type arc () =

    static member connect (source: Transform, dest: Transform) =
        let s = Node.Transform source
        let d = Node.Transform dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Transform, dest: Merge, proportion:float) =
        let s = Node.Transform source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Transform, dest: Split) =
        let s = Node.Transform source
        let d = Node.Split dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Transform, dest: Buffer) =
        let s = Node.Transform source
        let d = Node.Buffer dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Merge, dest: Transform) =
        let s = Node.Merge source
        let d = Node.Transform dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Merge, dest: Merge, proportion:float) =
        let s = Node.Merge source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Merge, dest: Split) =
        let s = Node.Merge source
        let d = Node.Split dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Merge, dest: Buffer) =
        let s = Node.Merge source
        let d = Node.Buffer dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Buffer, dest: Merge, proportion:float) =
        let s = Node.Buffer source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Buffer, dest: Split) =
        let s = Node.Buffer source
        let d = Node.Split dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Buffer, dest: Buffer) =
        let s = Node.Buffer source
        let d = Node.Buffer dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Buffer, dest: Transform) =
        let s = Node.Buffer source
        let d = Node.Transform dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Split, dest: Transform, proportion: float) =
        let s = Node.Split source
        let d = Node.Transform dest
        Arc.create s d proportion

    static member connect (source: Split, dest: Buffer, proportion: float) =
        let s = Node.Split source
        let d = Node.Buffer dest
        Arc.create s d proportion

    static member connect (source: Split, dest: Merge, proportion: float) =
        let s = Node.Split source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Split, dest: Split, proportion: float) =
        let s = Node.Split source
        let d = Node.Split dest
        Arc.create s d proportion
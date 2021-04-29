[<AutoOpen>]
module DiscreteRate.Domain

open System

[<RequireQualifiedAccess>]
module Label =

    let create label =
        if String.IsNullOrEmpty label then
            invalidArg (nameof label) $"Cannot have a null or empty Label"

        Label label


[<RequireQualifiedAccess>]
module FlowRate =

    let create flowRate =
        if flowRate <= 0.0 then
            invalidArg (nameof flowRate) $"Cannot have a FlowRate less than or equal to 0.0"

        if flowRate = infinity then
            invalidArg (nameof flowRate) $"Cannot have a FlowRate of infinity"

        FlowRate flowRate


[<RequireQualifiedAccess>]
module FillRate =

    let create fillRate =
        if fillRate <= 0.0 then
            invalidArg (nameof fillRate) $"Cannot have a FillRate less than or equal to 0.0"

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
module TankLevel =

    let create tankLevel =
        if tankLevel < 0.0 then
            invalidArg (nameof tankLevel) $"Cannot have a TankLevel less than 0.0"

        if tankLevel = infinity then
            TankLevel.Infinite
        else
            TankLevel.Finite tankLevel


[<RequireQualifiedAccess>]
module Proportion =

    let create proportion =
        if proportion <= 0.0 then
            invalidArg (nameof proportion) $"Cannot have a Proportion <= 0.0"

        if proportion = infinity then
            invalidArg (nameof proportion) $"Cannot have a Proportion of infinity"

        Proportion proportion


[<RequireQualifiedAccess>]
module Operation =

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
module Tank =

    let create label maxLevel =
        if maxLevel < 0.0 then
            invalidArg (nameof maxLevel) $"Cannot have a {nameof maxLevel} < 0.0"

        {
            Label = Label.create label
            MaxLevel = TankLevel.create maxLevel
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

    let ofOperation operation =
        Node.Operation operation

    let ofTank tank =
        Node.Tank tank

    let ofMerge merge =
        Node.Merge merge

    let ofSplit split =
        Node.Split split

    let isProcess (n: Node) =
        match n with
        | Node.Operation _ -> true
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

    static member connect (source: Operation, dest: Operation) =
        let s = Node.Operation source
        let d = Node.Operation dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Operation, dest: Merge, proportion:float) =
        let s = Node.Operation source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Operation, dest: Split) =
        let s = Node.Operation source
        let d = Node.Split dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Operation, dest: Tank) =
        let s = Node.Operation source
        let d = Node.Tank dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Merge, dest: Operation) =
        let s = Node.Merge source
        let d = Node.Operation dest
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

    static member connect (source: Merge, dest: Tank) =
        let s = Node.Merge source
        let d = Node.Tank dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Tank, dest: Merge, proportion:float) =
        let s = Node.Tank source
        let d = Node.Merge dest
        Arc.create s d proportion

    static member connect (source: Tank, dest: Split) =
        let s = Node.Tank source
        let d = Node.Split dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Tank, dest: Tank) =
        let s = Node.Tank source
        let d = Node.Tank dest
        let p = 1.0
        Arc.create s d p

    static member connect (source: Tank, dest: Operation) =
        let s = Node.Tank source
        let d = Node.Operation dest
        let p = 1.0
        Arc.create s d p
module ss13.Clock

open System.Diagnostics

type Clock () =
    let frequency = Stopwatch.Frequency
    let mutable lastFrame = 0L
    let mutable initialTick = 0L

    member this.Start () =
        initialTick <- Stopwatch.GetTimestamp ()

    member this.Frame =
        let tick = Stopwatch.GetTimestamp ()
        let elapsed = (tick - lastFrame) / frequency
        lastFrame <- tick
        float elapsed

    member this.TotalTime =
        let tick = Stopwatch.GetTimestamp ()
        (tick - initialTick) / frequency
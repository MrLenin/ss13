module Main

open System
open SFML.Window
open SFML.Graphics
open SFML.System

let private window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let private clock = new Clock ()
let private timePerFrame = Time.FromSeconds (1.f / 60.f)

let mutable private timeSinceLastUpdate = Time.Zero

let rec private render () = async {
    match window.IsOpen with
    | false -> return ()
    | true -> 
        window.Clear ()
        window.Display ()
        return! render () }

let update dt =
    ()

let rec private run () =
    match window.IsOpen with
    | false -> ()
    | true ->
        window.DispatchEvents ()
        timeSinceLastUpdate <- timeSinceLastUpdate + clock.Restart()

        match timeSinceLastUpdate.AsMilliseconds () > timePerFrame.AsMilliseconds () with
        | true -> 
            timeSinceLastUpdate <- timeSinceLastUpdate - timePerFrame
            window.DispatchEvents ()
            update timePerFrame
            run ()
        | false -> run ()

let private OnClose sender args =
    window.Close ()

let private OnResized sender (args : SizeEventArgs) =
    ()

let private OnLostFocus sender args =
    ()

let private OnGainedFocus sender args =
    ()

[<EntryPoint>]
let main argv = 
    window.Closed.AddHandler (new EventHandler (OnClose))
    window.Resized.AddHandler (new EventHandler<SizeEventArgs> (OnResized))
    window.LostFocus.AddHandler (new EventHandler (OnLostFocus))
    window.GainedFocus.AddHandler (new EventHandler (OnGainedFocus))

    window.SetActive false |> ignore

    Async.Start (render ())
    run ()

    0

module Main

open System
open SFML.Window
open SFML.Graphics
open SFML.System
open State

let private window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let private clock = new Clock ()
let private timePerFrame = Time.FromSeconds (1.f / 60.f)

let context = new Context (window)
let stateStack = new StateStack (context)

let mutable private timeSinceLastUpdate = Time.Zero

let rec private render () = async {
    match window.IsOpen with
    | false -> return ()
    | true -> 
        window.Clear ()
        stateStack.Draw ()
        window.Display ()
        return! render () }

let update dt =
    stateStack.Update dt

let rec private processInput () =
    let mutable event = new Event ()

    match window.PollEvent (ref event) with
    | false -> ()
    | true ->
        stateStack.HandleEvent event
//        match event.Type with
//        | EventType.Closed -> match window.Closed with | null -> () 
        processInput ()
    
let rec private run () =
    match window.IsOpen with
    | false -> ()
    | true ->
        timeSinceLastUpdate <- timeSinceLastUpdate + clock.Restart()
        match timeSinceLastUpdate.AsMilliseconds () > timePerFrame.AsMilliseconds () with
        | true -> 
            timeSinceLastUpdate <- timeSinceLastUpdate - timePerFrame
            window.DispatchEvents ()
            processInput ()
            update timePerFrame
            if stateStack.IsEmpty then window.Close ()
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

let registerStates () =
    stateStack.RegisterState ID.Title (new CreateInstance (TitleState.CreateInstance))

[<EntryPoint>]
let main argv = 
    window.Closed.AddHandler (new EventHandler (OnClose))
    window.Resized.AddHandler (new EventHandler<SizeEventArgs> (OnResized))
    window.LostFocus.AddHandler (new EventHandler (OnLostFocus))
    window.GainedFocus.AddHandler (new EventHandler (OnGainedFocus))

    registerStates ()
    stateStack.Push ID.Title

    window.SetActive false |> ignore

    Async.Start (render ())
    run ()

    0

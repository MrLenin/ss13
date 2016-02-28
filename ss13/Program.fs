module Main

open System

open SFML.System

open State
open Events

let private clock = new Clock ()
let private timePerFrame = Time.FromSeconds (1.f / 60.f)

let mutable private timeSinceLastUpdate = Time.Zero

let update dt = stateStack.Update dt

let rec private render () = async {
    match window.IsOpen with
    | false -> return ()
    | true -> 
        window.Clear ()
        stateStack.Draw ()
        window.Display ()
        return! render () }

let rec private run () =
    match window.IsOpen with
    | false -> ()
    | true ->
        timeSinceLastUpdate <- timeSinceLastUpdate + clock.Restart()
        match timeSinceLastUpdate.AsMilliseconds () > timePerFrame.AsMilliseconds () with
        | true -> 
            timeSinceLastUpdate <- timeSinceLastUpdate - timePerFrame
            window.DispatchEvents ()
            update timePerFrame
            if stateStack.IsEmpty then window.Close ()
        | false -> ()
        run ()

let registerStates () =
    stateStack.RegisterState ID.Title (new CreateInstance (TitleState.CreateInstance))

[<EntryPoint>]
let main argv = 
    registerStates ()
    stateStack.Push ID.Title
    window.SetActive false |> ignore

    Async.Start (render ())
    run ()

    0

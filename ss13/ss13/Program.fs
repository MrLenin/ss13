module Main

open System
open SFML.Window
open SFML.Graphics
open SFML.System

type Key = Keyboard.Key

let private window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let private clock = new Clock ()
let private timePerFrame = Time.FromSeconds (1.f / 60.f)
let private player = new CircleShape ()

let mutable private timeSinceLastUpdate = Time.Zero
let mutable private movingUp = false
let mutable private movingDown = false
let mutable private movingLeft = false
let mutable private movingRight = false

let rec private render () = async {
    match window.IsOpen with
    | false -> return ()
    | true -> 
        window.Clear ()
        window.Draw player
        window.Display ()
        return! render () }

let update (deltaTime : Time) =
    let mutable movement = Vector2f (0.f, 0.f)

    if movingUp then movement.Y <- movement.Y - 100.f
    if movingDown then movement.Y <- movement.Y + 100.f
    if movingLeft then movement.X <- movement.X - 100.f
    if movingRight then movement.X <- movement.X + 100.f

    let update = movement * deltaTime.AsSeconds ()
    player.Position <- player.Position + update

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

let private handlePlayerInput key pressed =
    match key with
    | Key.W -> movingUp <- pressed
    | Key.S -> movingDown <- pressed
    | Key.A -> movingLeft <- pressed
    | Key.D -> movingRight <- pressed
    | Key.Escape -> window.Close ()
    | _ -> ()

let private OnClose sender evargs =
    window.Close ()

let private OnKeyPressed sender (evargs : KeyEventArgs) =
    handlePlayerInput evargs.Code true

let private OnKeyReleased sender (evargs : KeyEventArgs) =
    handlePlayerInput evargs.Code false

[<EntryPoint>]
let main argv = 
    window.Closed.AddHandler (new EventHandler (OnClose))
    window.KeyPressed.AddHandler (new EventHandler<KeyEventArgs> (OnKeyPressed))
    window.KeyReleased.AddHandler (new EventHandler<KeyEventArgs> (OnKeyReleased))

    player.Radius <- 40.f
    player.Position <- Vector2f (100.f, 100.f)
    player.FillColor <- Color.Cyan

    window.SetActive false |> ignore

    Async.Start (render ())
    run ()

    0

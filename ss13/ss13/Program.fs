module Main

open System
open SFML.Window
open SFML.Graphics

let OnClose (sender : obj) evargs =
            // Close the window when OnClose event is received
            let window = sender :?> SFML.Graphics.RenderWindow
            window.Close();

let rec renderingThread (window : SFML.Graphics.RenderWindow) =
    async {
    // the rendering loop
    while window.IsOpen do
        window.Clear ()
        window.Display ()
        return! renderingThread window
    }

[<EntryPoint>]
let main argv = 
    let window = new SFML.Graphics.RenderWindow (SFML.Window.VideoMode (1280u, 720u), "ss13")
    window.Closed.AddHandler (new System.EventHandler(OnClose))
    window.SetActive false |> ignore
    Async.Start (renderingThread window)

    while window.IsOpen do
        window.DispatchEvents ()


    0 // return an integer exit code

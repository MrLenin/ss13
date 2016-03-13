module ss13.Program

open System
open System.Drawing
open System.Threading
open System.Windows.Forms

open SharpBgfx

open Clock
open EventQueue

let mutable windowWidth = 800
let mutable windowHeight = 600

let eventQueue = new EventQueue ()
let clock = new Clock()
let form = new Form ()

let mre = new ManualResetEventSlim (false);

form.Text <- "SS13"
form.ClientSize <- new Size (windowWidth, windowHeight)
form.ClientSizeChanged.Add (fun evArgs -> eventQueue.Post (new SizeEvent (windowWidth, windowHeight)))
form.FormClosed.Add (fun evArgs -> eventQueue.Post (new Event (ExitEvent)))
form.FormClosing.Add (fun evArgs ->
    eventQueue.Post (new Event(ExitEvent))
    mre.Wait () |> ignore)

Bgfx.SetWindowHandle form.Handle

let rec processEvents resetFlags resizeRequired =
    let ev = eventQueue.Poll ()

    match ev with
    | None ->
        if resizeRequired then
            Bgfx.Reset (windowWidth, windowHeight, resetFlags)
        true
    | Some event ->
        match event.Type with
        | ExitEvent ->
            false
        | SizeEvent ->
            let sizeEvent = (event :?> SizeEvent)
            windowWidth <- sizeEvent.Width
            windowHeight <- sizeEvent.Height
            processEvents resetFlags true
        | _ -> processEvents resetFlags resizeRequired

let rec private render () =
    if not (processEvents ResetFlags.Vsync false) then
        ()
    else
        Bgfx.SetViewRect (0uy, 0, 0, windowWidth, windowHeight)
        Bgfx.Touch 0uy |> ignore
        let elapsed = clock.Frame
        let time = clock.TotalTime
        Bgfx.DebugTextClear ()
        Bgfx.DebugTextWrite (0, 1, DebugColor.White, DebugColor.Blue, "SS13")
        Bgfx.DebugTextWrite (0, 2, DebugColor.White, DebugColor.Cyan, "Description: Initialization and debug text.")
        Bgfx.DebugTextWrite (0, 3, DebugColor.White, DebugColor.Cyan, "Frame: {0:F3} ms", elapsed * 1000.0)
        Bgfx.Frame () |> ignore
        render ()

let private renderWorkflow () = async {
    Bgfx.Init ()
    Bgfx.Reset (windowWidth, windowHeight, ResetFlags.Vsync)
    Bgfx.SetDebugFeatures DebugFeatures.DisplayText
    Bgfx.SetViewClear (0uy, ClearTargets.Color ||| ClearTargets.Depth, 0x303030ff)
    clock.Start ()
    render ()
    Bgfx.Shutdown ()
    mre.Set () |> ignore
    }

[<EntryPoint>]
let main argv = 
    Async.Start (renderWorkflow ())
    Application.Run form

    0

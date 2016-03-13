module ss13.EventQueue

open System.Collections.Concurrent

type EventType =
    | ExitEvent
    | KeyEvent
    | MouseEvent
    | SizeEvent

type Event (eventType : EventType) =
    member val Type = eventType

type SizeEvent (width : int, height : int) =
    inherit Event (EventType.SizeEvent)
    member val Width = width
    member val Height = height

type EventQueue () =
    let queue = new ConcurrentQueue<Event> ()

    member this.Poll () = 
        let (res, event) = queue.TryDequeue ()
        if res then Some event else None

    member this.Post event =
        queue.Enqueue event
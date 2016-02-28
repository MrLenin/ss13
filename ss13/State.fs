﻿module State

open SFML.Graphics
open SFML.Window
open SFML.System

type ID =
    | Unassigned
    | Title
    | Menu
    | Game
    | Loading
    | Pause

type Action =
    | Push
    | Pop
    | Clear

type EventArgs =
    | Args of System.EventArgs
    | SizeArgs of SizeEventArgs
    | TextArgs of TextEventArgs
    | KeyArgs of KeyEventArgs
    | MouseMoveArgs of MouseMoveEventArgs
    | MouseButtonArgs of MouseButtonEventArgs
    | MouseWheelScrollArgs of MouseWheelScrollEventArgs
    | JoystickButtonArgs of JoystickButtonEventArgs
    | JoystickMoveArgs of JoystickMoveEventArgs
    | JoystickConnectArgs of JoystickConnectEventArgs

type Context =
    struct
        val Window : RenderWindow
        new (window) = { Window = window }
    end

type PendingChange =
    struct
        val Action : Action
        val ID : ID
        new (action, id) = { Action = action; ID = id }
        new (action) = { Action = action; ID = ID.Unassigned }
    end

[<AbstractClass>]
type State (stack : StateStack, context : Context) =
    let stack = stack
    let mutable context = context

    member val Context = context

    member this.StackClear () = stack.Clear ()
    member this.StackPop () = stack.Pop ()
    member this.StackPush id = stack.Push id

    abstract Draw : unit -> unit
    abstract Update : Time -> bool
    abstract HandleEvent : EventArgs -> bool

and StateStack (context : Context) =
    let context = context

    let mutable stack = List<State>.Empty
    let mutable pendingList = List<PendingChange>.Empty
    let mutable factories : Map<ID, CreateInstance> = Map.empty

    member this.IsEmpty = stack.IsEmpty

    member private this.applyPendingChanges () =
        for change in pendingList do
            match change.Action with
            | Clear -> stack <- List<State>.Empty
            | Pop -> stack <- (stack |> List.rev).Tail |> List.rev
            | Push ->
                match this.createState change.ID with
                | Some state -> stack <- stack @ [state]
                | None -> ()
        pendingList <- List<PendingChange>.Empty

    member private this.createState id : State option =
        match factories.TryFind id with
        | Some factory -> Some (factory.Invoke (this, context))
        | None -> None

    member this.Clear () =
        pendingList <- pendingList @ [PendingChange(Clear)]

    member this.Draw () =
        for state in stack do
            state.Draw ()

    member this.HandleEvent args =
        List.tryFind (fun (state : State) ->
        not (state.HandleEvent args)) (stack |> List.rev)
        |> ignore
        this.applyPendingChanges ()

    member this.Pop () =
        pendingList <- pendingList @ [PendingChange(Pop)]

    member this.Push id =
        pendingList <- pendingList @ [PendingChange(Push, id)]

    member this.RegisterState id createInstance =
        factories <- factories |> Map.add id createInstance

    member this.Update dt =
        List.tryFind (fun (state : State) ->
        not (state.Update dt)) (stack |> List.rev)
        |> ignore
        this.applyPendingChanges ()

and CreateInstance = delegate of (StateStack * Context) -> State

type TitleState (stack, context) =
    inherit State (stack, context)

    static member CreateInstance (stack, context) = new TitleState (stack, context) :> State

    override this.Draw () = ()
    override this.Update dt = false
    override this.HandleEvent event = false


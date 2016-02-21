module Scene

open SFML.Graphics
open SFML.System

open Command

[<AbstractClass>]
type SceneNode () as this =
    inherit Transformable ()

    [<DefaultValue>]
    val mutable private parent : SceneNode option

    let mutable children = List<SceneNode>.Empty

    let drawChildren target states =
        for child in children do
            (child :> Drawable).Draw (target, states)

    let updateChildren dt =
        for child in children do
            child.Update dt

    let rec getWorldTransform transform node =
        match node with
        | None -> transform
        | Some (node : SceneNode) -> getWorldTransform (node.Transform * transform) node.parent

    interface Drawable with
        override this.Draw (target, states) =
            let mutable states = states
            states.Transform <- states.Transform * this.Transform
            this.DrawCurrent target states
            drawChildren target states

    member val WorldTransform : Transform = getWorldTransform Transform.Identity this.parent

    member val WorldPosition = this.WorldTransform * Vector2f ()
    
    abstract Category : Category with get
    default this.Category = Category.None
    
    member this.AttachChild (child : SceneNode) =
        child.parent <- Some this
        children <- children @ [child]
        
    member this.DetachChild (child : SceneNode) =
        child.parent <- None
        let newChildren, _ = List.partition (fun elem -> not (elem = child)) children
        children <- newChildren

    member this.OnCommand (cmd : Command<SceneNode>) (dt : Time) =
        if (cmd.Category &&& this.Category) = this.Category then
            cmd.Action.Invoke (this, dt)
        
        for child in children do
            child.OnCommand cmd dt

    member this.Update dt =
        this.UpdateCurrent dt
        updateChildren dt

    abstract member DrawCurrent : RenderTarget -> RenderStates -> unit

    abstract member UpdateCurrent : Time -> unit


type CommandQueue = Fifo<Command<SceneNode>>

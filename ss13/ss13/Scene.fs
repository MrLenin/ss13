module Scene

open SFML.Graphics
open SFML.System

[<AbstractClass>]
type SceneNode () as this =
    inherit Transformable ()

    [<DefaultValue>]
    val mutable private parent : SceneNode option

    let mutable children = List<SceneNode>.Empty

    let drawChildren target states =
        for child in children do
            (child :> Drawable).Draw (target, states)

    let updateChildren deltaTime =
        for child in children do
            child.Update deltaTime

    let rec getWorldTransform transform node =
        match node with
        | None -> transform
        | Some (node : SceneNode) -> getWorldTransform (node.Transform * transform) node.parent

    interface Drawable with
        override this.Draw (target, states) =
            let mutable newStates = states
            newStates.Transform <- states.Transform * this.Transform
            this.DrawCurrent target newStates
            drawChildren target newStates

    member val WorldTransform : Transform = getWorldTransform Transform.Identity this.parent

    member val WorldPosition = this.WorldTransform * Vector2f ()
    
    member this.AttachChild (child : SceneNode) =
        child.parent <- Some this
        children <- children @ [child]
        
    member this.DetachChild (child : SceneNode) =
        child.parent <- None
        let newChildren, _ = List.partition (fun elem -> not (elem = child)) children
        children <- newChildren

    member this.Update (deltaTime : Time) =
        this.UpdateCurrent deltaTime
        updateChildren deltaTime

    abstract member DrawCurrent : RenderTarget -> RenderStates -> unit

    abstract member UpdateCurrent : Time -> unit



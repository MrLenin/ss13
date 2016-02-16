module Scene

open SFML.Graphics
open SFML.System

type SceneNode () =
    inherit Transformable ()

    let mutable children = List<SceneNode>.Empty

    let drawCurrent target states = ()

    let updateCurrent deltaTime = ()

    let drawChildren target states =
        for child in children do
            (child :> Drawable).Draw (target, states)

    let updateChildren deltaTime =
        for child in children do
            child.Update deltaTime

    do ()

    interface Drawable with
        member this.Draw (target, states) =
            let mutable newStates = states
            newStates.Transform <- states.Transform * this.Transform
            drawCurrent target newStates
            drawChildren target newStates

    member val Parent =  None : SceneNode option with get, set
    
    member this.AttachChild (child : SceneNode) =
        child.Parent <- Some this
        children <- children @ [child]
        
    member this.DetachChild (child : SceneNode) =
        child.Parent <- None
        let newChildren, _ = List.partition (fun elem -> not (elem = child)) children
        children <- newChildren

    member this.GetWorldTransform () =
        let mutable transform = Transform.Identity
        let mutable node = Some this
        while not (node = None) do
            transform <- node.Value.Transform * transform
            node <- node.Value.Parent
        transform

    member this.GetWorldPosition () =
        this.GetWorldTransform () * Vector2f ()

    member this.Update (deltaTime : Time) =
        updateCurrent deltaTime
        updateChildren deltaTime



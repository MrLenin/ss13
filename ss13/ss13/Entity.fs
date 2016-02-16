module Entity

open SFML.Graphics
open SFML.System

type Entity () as this =
    inherit Scene.SceneNode ()

    let updateCurrent deltaTime = (this :> Transformable).Position <- this.Velocity * deltaTime

    do ()

    member val Velocity = Vector2f () with get, set
module Entity

open SFML.Graphics
open SFML.System

type Entity () as this =
    inherit Scene.SceneNode ()

    let updateCurrent deltaTime =
        let transform = (this :> Transformable)
        let update = this.Velocity * deltaTime
        transform.Position <- transform.Position + update

    do ()

    member val Velocity = Vector2f () with get, set
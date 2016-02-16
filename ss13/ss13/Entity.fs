module Entity

open SFML.Graphics
open SFML.System

type Entity () =
    inherit Scene.SceneNode ()

    do ()

    member val Velocity = Vector2f () with get, set

    override this.UpdateCurrent deltaTime =
        let transform = (this :> Transformable)
        let update = this.Velocity * deltaTime.AsSeconds ()
        transform.Position <- transform.Position + update

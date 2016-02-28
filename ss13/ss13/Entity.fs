module Entity

open SFML.Graphics
open SFML.System

[<AbstractClass>]
type Entity () =
    inherit Scene.SceneNode ()

    let mutable velocity = Vector2f ()

    member this.Velocity with get () = velocity and set value = velocity <- value

    override this.UpdateCurrent deltaTime =
        let transform = (this :> Transformable)
        let update = this.Velocity * deltaTime.AsSeconds ()
        transform.Position <- transform.Position + update

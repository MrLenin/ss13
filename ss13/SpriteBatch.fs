module ss13.SpriteBatch

open System
open System.Drawing
open SharpBgfx

open MathTypes

type SpriteSortMode =
    | SortModeDeferred
    | SortModeImmediate
    | SortModeTexture
    | SortModeBackToFront
    | SortModeFrontToBack

[<Flags>]
type SpriteEffects =
    | SpriteEffectsNone = 0
    | SpriteEffectsFlipHorizontally = 1
    | SpriteEffectsFlipVertically = 2
    | SpriteEffectsFlipBoth = 3

type Sprite =
    {   AssetName : string
        Texture : Texture
        Position : Vector2
        Scale : float
        Size : Rectangle    }
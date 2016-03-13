module ss13.VertexTypes

open System

open SharpBgfx

[<Struct>]
type PosVertex =
    val x : float
    val y : float
    val z : float

    new (x, y, z) =
        { x = x ; y = y ; z = z }

    static member Layout =
        VertexLayout().Begin()
            .Add(VertexAttributeUsage.Position, 3, VertexAttributeType.Float)
            .End()

[<Struct>]
type PosColorVertex =
    val x : float
    val y : float
    val z : float
    val abgr : uint32

    new (x, y, z, abgr) =
        { x = x ; y = y ; z = z ; abgr = abgr }

    static member Layout =
        VertexLayout().Begin()
            .Add(VertexAttributeUsage.Position, 3, VertexAttributeType.Float)
            .Add(VertexAttributeUsage.Color0, 4, VertexAttributeType.UInt8, true)
            .End()

[<Struct>]
type PosColorTexcoordVertex =
    val x : float
    val y : float
    val z : float
    val abgr : uint32
    val u : float
    val v : float

    new (x, y, z, abgr, u, v) =
        { x = x ; y = y ; z = z ; abgr = abgr ; u = u ; v = v  }

    static member Layout =
        VertexLayout().Begin()
            .Add(VertexAttributeUsage.Position, 3, VertexAttributeType.Float)
            .Add(VertexAttributeUsage.Color0, 4, VertexAttributeType.UInt8, true)
            .Add(VertexAttributeUsage.TexCoord0, 2, VertexAttributeType.Float)
            .End()

[<Struct>]
type PosTexcoordVertex =
    val x : float
    val y : float
    val z : float
    val u : float
    val v : float

    new (x, y, z, u, v) =
        { x = x ; y = y ; z = z ; u = u ; v = v  }

    static member Layout =
        VertexLayout().Begin()
            .Add(VertexAttributeUsage.Position, 3, VertexAttributeType.Float)
            .Add(VertexAttributeUsage.TexCoord0, 2, VertexAttributeType.Float)
            .End()

[<Struct>]
type PosNormalTexcoordVertex =
    val x : float
    val y : float
    val z : float
    val normal : uint32
    val u : float
    val v : float

    new (x, y, z, normal, u, v) =
        { x = x; y = y; z = z; normal = normal; u = u; v = v }

    static member Layout =
        VertexLayout().Begin()
            .Add(VertexAttributeUsage.Position, 3, VertexAttributeType.Float)
            .Add(VertexAttributeUsage.Normal, 4, VertexAttributeType.UInt8, true, true)
            .Add(VertexAttributeUsage.TexCoord0, 2, VertexAttributeType.Float)
            .End()
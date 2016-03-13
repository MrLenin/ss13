namespace ss13

open System
open System.Globalization

open MathTypes

module Vector2 =
    let Zero : Vector2  = { X = 0.0 ; Y = 0.0 }
    let One : Vector2   = { X = 1.0 ; Y = 1.0 }
    let UnitX : Vector2 = { X = 1.0 ; Y = 0.0 }
    let UnitY : Vector2 = { X = 0.0 ; Y = 1.0 }

    let Distance (v1 : Vector2) (v2 : Vector2) =
        let dx, dy =
            v1.X - v2.X,
            v1.Y - v2.Y
        sqrt (dx * dx + dy * dy)

    let DistanceSquared (v1 : Vector2) (v2 : Vector2) =
        let dx, dy =
            v1.X - v2.X,
            v1.Y - v2.Y
        dx * dx + dy * dy

    let Dot (v1 : Vector2) (v2 : Vector2) =
        v1.X * v2.X + v1.Y * v2.Y

    let Normalize (v : Vector2) : Vector2 =
        let invNorm = 1.0 / sqrt (v.X * v.X + v.Y * v.Y)
        {   X = v.X * invNorm ; Y = v.Y * invNorm   }

    let PerpDot (v1 : Vector2) (v2 : Vector2) =
        v1.X * v2.Y - v1.Y * v2.X

    let Reflect (v : Vector2) (n : Vector2) : Vector2 =
        let dot = v.X * n.X + v.Y * n.Y
        {   X = v.X - 2.0 * dot * n.X
            Y = v.Y - 2.0 * dot * n.Y   }

    let Min (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = (if v1.X < v2.X then v1.X else v2.X)
            Y = (if v1.Y < v2.Y then v1.Y else v2.Y)   }

    let Max (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = (if v1.X > v2.X then v1.X else v2.X)
            Y = (if v1.Y > v2.Y then v1.Y else v2.Y)   }

    let Clamp (v : Vector2) (min : Vector2) (max : Vector2) : Vector2 = 
        // This compare order is very important!!!
        // We must follow HLSL behavior in the case user specfied min value is bigger than max value.
        let mutable x = v.X
        let mutable y = v.Y
        x <- if x > max.X then max.X else x
        x <- if x < min.X then min.X else x
        y <- if y > max.Y then max.Y else y
        y <- if y < min.Y then min.Y else y
        {   X = x ; Y = y   }

    let Transform3x2 (position : Vector2) (matrix : Matrix3x2) : Vector2 =
        {   X = position.X * matrix.M11 + position.Y * matrix.M21 + matrix.M31
            Y = position.X * matrix.M12 + position.Y * matrix.M22 + matrix.M32   }

    let Transform4x4 (position : Vector2) (matrix : Matrix4x4) : Vector2 =
        {   X = position.X * matrix.M11 + position.Y * matrix.M21 + matrix.M41
            Y = position.X * matrix.M12 + position.Y * matrix.M22 + matrix.M42   }

    let TransformNormal3x2 (normal : Vector2) (matrix : Matrix3x2) : Vector2 =
        {   X = normal.X * matrix.M11 + normal.Y * matrix.M21
            Y = normal.X * matrix.M12 + normal.Y * matrix.M22   }

    let TransformNormal4x4 (normal : Vector2) (matrix : Matrix4x4) : Vector2 =
        {   X = normal.X * matrix.M11 + normal.Y * matrix.M21
            Y = normal.X * matrix.M12 + normal.Y * matrix.M22   }

    let Transform (value : Vector2) (rotation : Quaternion) : Vector2 =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wz2, xx2, xy2, yy2, zz2 =
            rotation.W * z2, rotation.X * x2,
            rotation.X * y2, rotation.Y * y2,
            rotation.Z * z2

        {   X = value.X * (1.0 - yy2 - zz2) + value.Y * (xy2 - wz2)
            Y = value.X * (xy2 + wz2) + value.Y * (1.0 - xx2 - zz2)    }

    let Lerp (v1 : Vector2) (v2 : Vector2) a : Vector2 =
        {   X = v1.X + (v2.X - v1.X) * a
            Y = v1.Y + (v2.Y - v1.Y) * a   }

    let Negate (v : Vector2) : Vector2 =
        {   X = -v.X ; Y = -v.Y   }

    let Add (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = v1.X + v2.X ; Y = v1.Y + v2.Y   }

    let Subtract (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y   }

    let Multiply (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = v1.X * v2.X ; Y = v1.Y * v2.Y   }

    let MultiplyFloat (v1 : Vector2) (v2 : float) : Vector2 =
        {   X = v1.X * v2 ; Y = v1.Y * v2   }

    let Divide (v1 : Vector2) (v2 : Vector2) : Vector2 =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y   }

    let DivideFloat (v1 : Vector2) (v2 : float) : Vector2 =
        let invDiv = 1.0 / v2
        {   X = v1.X * invDiv ; Y = v1.Y * invDiv   }

module Vector3 =
    let Zero : Vector3  = { X = 0.0 ; Y = 0.0 ; Z = 0.0 }
    let One : Vector3   = { X = 1.0 ; Y = 1.0 ; Z = 1.0 }
    let UnitX : Vector3 = { X = 1.0 ; Y = 0.0 ; Z = 0.0 }
    let UnitY : Vector3 = { X = 0.0 ; Y = 1.0 ; Z = 0.0 }
    let UnitZ : Vector3 = { X = 0.0 ; Y = 0.0 ; Z = 1.0 }

    let Distance (v1 : Vector3) (v2 : Vector3) =
        let dx, dy, dz = v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z
        sqrt (dx * dx + dy * dy + dz * dz)

    let DistanceSquared (v1 : Vector3) (v2 : Vector3) =
        let dx, dy, dz = v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z
        dx * dx + dy * dy + dz * dz

    let Cross (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = v1.Y * v2.Z - v1.Z * v2.Y
            Y = v1.Z * v2.X - v1.X * v2.Z
            Z = v1.X * v2.Y - v1.Y * v2.X   }

    let Dot (v1 : Vector3) (v2 : Vector3) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    let Normalize (v : Vector3) : Vector3 =
        let invNorm = 1.0 / sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
        {   X = v.X * invNorm ; Y = v.Y * invNorm ; Z = v.Z * invNorm   }

    let Reflect (v : Vector3) (n : Vector3) : Vector3 =
        let dot = v.X * n.X + v.Y * n.Y + v.Z * n.Z
        {   X = v.X - 2.0 * dot * n.X
            Y = v.Y - 2.0 * dot * n.Y
            Z = v.Z - 2.0 * dot * n.Z   }

    let Min (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = if v1.X < v2.X then v1.X else v2.X
            Y = if v1.Y < v2.Y then v1.Y else v2.Y
            Z = if v1.Z < v2.Z then v1.Z else v2.Z   }

    let Max (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = if v1.X > v2.X then v1.X else v2.X
            Y = if v1.Y > v2.Y then v1.Y else v2.Y
            Z = if v1.Z > v2.Z then v1.Z else v2.Z   }

    let Clamp (v : Vector3) (min : Vector3) (max : Vector3) : Vector3 = 
        // This compare order is very important!!!
        // We must follow HLSL behavior in the case user specfied min value is bigger than max value.
        let mutable x = v.X
        let mutable y = v.Y
        let mutable z = v.Z
        x <- if x > max.X then max.X else x
        x <- if x < min.X then min.X else x
        y <- if y > max.Y then max.Y else y
        y <- if y < min.Y then min.Y else y
        z <- if z > max.Z then max.Z else z
        z <- if z < min.Z then min.Z else z
        {   X = x ; Y = y ; Z = z   }

    let Lerp (v1 : Vector3) (v2 : Vector3) (a : float) : Vector3 =
        {   X = v1.X + (v2.X - v1.X) * a
            Y = v1.Y + (v2.Y - v1.Y) * a
            Z = v1.Z + (v2.Z - v1.Z) * a   }

    let Transform4x4 (position : Vector3) (matrix : Matrix4x4) : Vector3 =
        {   X = position.X * matrix.M11 + position.Y * matrix.M21 + position.Z * matrix.M31 + matrix.M41
            Y = position.X * matrix.M12 + position.Y * matrix.M22 + position.Z * matrix.M32 + matrix.M42
            Z = position.X * matrix.M13 + position.Y * matrix.M23 + position.Z * matrix.M33 + matrix.M43   }


    let TransformNormal (normal : Vector3) (matrix : Matrix4x4) : Vector3 =
        {   X = normal.X * matrix.M11 + normal.Y * matrix.M21 + normal.Z * matrix.M31
            Y = normal.X * matrix.M12 + normal.Y * matrix.M22 + normal.Z * matrix.M32
            Z = normal.X * matrix.M13 + normal.Y * matrix.M23 + normal.Z * matrix.M33   }
      
    let Transform (value : Vector3) (rotation : Quaternion) : Vector3 =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wx2, wy2, wz2, xx2, xy2, xz2, yy2, yz2, zz2 =
            rotation.W * x2, rotation.W * y2, rotation.W * z2,
            rotation.X * x2, rotation.X * y2, rotation.X * z2,
            rotation.Y * y2, rotation.Y * z2, rotation.Z * z2
            
        {   X = value.X * (1.0 - yy2 - zz2) + value.Y * (xy2 - wz2) + value.Z * (xz2 + wy2)
            Y = value.X * (xy2 + wz2) + value.Y * (1.0 - xx2 - zz2) + value.Z * (yz2 - wx2)
            Z = value.X * (xz2 - wy2) + value.Y * (yz2 + wx2) + value.Z * (1.0 - xx2 - yy2)   }

    let Negate (v : Vector3) : Vector3 =
        {   X = -v.X ; Y = -v.Y ; Z = -v.Z   }

    let Add (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z   }

    let Subtract (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z   }

    let Multiply (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = v1.X * v2.X ; Y = v1.Y * v2.Y ; Z = v1.Z * v2.Z   }

    let MultiplyFloat (v1 : Vector3) (v2 : float) : Vector3 =
        {   X = v1.X * v2 ; Y = v1.Y * v2 ; Z = v1.Z * v2   }

    let Divide (v1 : Vector3) (v2 : Vector3) : Vector3 =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y ; Z = v1.Z / v2.Z   }

    let DivideFloat (v1 : Vector3) (v2 : float) : Vector3 =
        let invDiv = 1.0 / v2
        {   X = v1.X * invDiv ; Y = v1.Y * invDiv ; Z = v1.Z / v2  }

module Vector4 =
    let Zero : Vector4  = { X = 0.0 ; Y = 0.0 ; Z = 0.0 ; W = 0.0 }
    let One : Vector4   = { X = 1.0 ; Y = 1.0 ; Z = 1.0 ; W = 1.0 }
    let UnitX : Vector4 = { X = 1.0 ; Y = 0.0 ; Z = 0.0 ; W = 0.0 }
    let UnitY : Vector4 = { X = 0.0 ; Y = 1.0 ; Z = 0.0 ; W = 0.0 }
    let UnitZ : Vector4 = { X = 0.0 ; Y = 0.0 ; Z = 1.0 ; W = 0.0 }
    let UnitW : Vector4 = { X = 0.0 ; Y = 0.0 ; Z = 0.0 ; W = 1.0 }

    let Distance (v1 : Vector4) (v2 : Vector4) =
        let dx, dy, dz, dw = v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W
        sqrt (dx * dx + dy * dy + dz * dz + dw * dw)

    let DistanceSquared (v1 : Vector4) (v2 : Vector4) =
        let dx, dy, dz, dw = v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W
        dx * dx + dy * dy + dz * dz + dw * dw

    let Dot (v1 : Vector4) (v2 : Vector4) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z + v1.W * v2.W

    let Normalize (v : Vector4) : Vector4 =
        let invNorm = 1.0 / sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z + v.W * v.W)
        {   X = v.X * invNorm
            Y = v.Y * invNorm
            Z = v.Z * invNorm
            W = v.W * invNorm   }

    let Reflect (v : Vector4) (n : Vector4) : Vector4 =
        let dot = v.X * n.X + v.Y * n.Y + v.Z * n.Z + v.W * n.W
        {   X = v.X - 2.0 * dot * n.X
            Y = v.Y - 2.0 * dot * n.Y
            Z = v.Z - 2.0 * dot * n.Z
            W = v.W - 2.0 * dot * n.W   }

    let Min (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = if v1.X < v2.X then v1.X else v2.X
            Y = if v1.Y < v2.Y then v1.Y else v2.Y
            Z = if v1.Z < v2.Z then v1.Z else v2.Z
            W = if v1.W < v2.W then v1.W else v2.W   }

    let Max (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = if v1.X > v2.X then v1.X else v2.X
            Y = if v1.Y > v2.Y then v1.Y else v2.Y
            Z = if v1.Z > v2.Z then v1.Z else v2.Z
            W = if v1.W > v2.W then v1.W else v2.W   }

    let Clamp (v : Vector4) (min : Vector4) (max : Vector4) : Vector4 = 
        // This compare order is very important!!!
        // We must follow HLSL behavior in the case user specfied min value is bigger than max value.
        let mutable x, y, z, w = v.X, v.Y, v.Z, v.W
        x <- if x > max.X then max.X else x
        x <- if x < min.X then min.X else x
        y <- if y > max.Y then max.Y else y
        y <- if y < min.Y then min.Y else y
        z <- if z > max.Z then max.Z else z
        z <- if z < min.Z then min.Z else z
        w <- if z > max.W then max.W else w
        w <- if w < min.W then min.W else w
        {   X = x ; Y= y ; Z = z ; W = w   }

    let Lerp (v1 : Vector4) (v2 : Vector4) (a : float) : Vector4 =
        {   X = v1.X + (v2.X - v1.X) * a
            Y = v1.Y + (v2.Y - v1.Y) * a
            Z = v1.Z + (v2.Z - v1.Z) * a
            W = v1.W + (v2.W - v1.W) * a   }

    let TransformVec2 (position : Vector2) (matrix : Matrix4x4) =
        {   X = position.X * matrix.M11 + position.Y * matrix.M21 + matrix.M41
            Y = position.X * matrix.M12 + position.Y * matrix.M22 + matrix.M42
            Z = position.X * matrix.M13 + position.Y * matrix.M23 + matrix.M43
            W = position.X * matrix.M14 + position.Y * matrix.M24 + matrix.M44   }


    let TransformVec3 (position : Vector3) (matrix : Matrix4x4) =
        {   X = position.X * matrix.M11 + position.Y * matrix.M21 + position.Z * matrix.M31 + matrix.M41
            Y = position.X * matrix.M12 + position.Y * matrix.M22 + position.Z * matrix.M32 + matrix.M42
            Z = position.X * matrix.M13 + position.Y * matrix.M23 + position.Z * matrix.M33 + matrix.M43
            W = position.X * matrix.M14 + position.Y * matrix.M24 + position.Z * matrix.M34 + matrix.M44   }


    let TransformVec4 (vector : Vector4) (matrix : Matrix4x4) =
        {   X = vector.X * matrix.M11 + vector.Y * matrix.M21 + vector.Z * matrix.M31 + vector.W * matrix.M41
            Y = vector.X * matrix.M12 + vector.Y * matrix.M22 + vector.Z * matrix.M32 + vector.W * matrix.M42
            Z = vector.X * matrix.M13 + vector.Y * matrix.M23 + vector.Z * matrix.M33 + vector.W * matrix.M43
            W = vector.X * matrix.M14 + vector.Y * matrix.M24 + vector.Z * matrix.M34 + vector.W * matrix.M44   }


    let TransformVec2Q (value : Vector2) (rotation : Quaternion) =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wx2, wy2, wz2,
            xx2, xy2, xz2,
            yy2, yz2, zz2 =
                rotation.W * x2, rotation.W * y2, rotation.W * z2,
                rotation.X * x2, rotation.X * y2, rotation.X * z2,
                rotation.Y * y2, rotation.Y * z2, rotation.Z * z2

        {   X = value.X * ( 1.0 - yy2 - zz2 ) + value.Y * (       xy2 - wz2 )
            Y = value.X * (       xy2 + wz2 ) + value.Y * ( 1.0 - xx2 - zz2 )
            Z = value.X * (       xz2 - wy2 ) + value.Y * (       yz2 + wx2 )
            W = 1.0   }


    let TransformVec3Q (value : Vector3) (rotation : Quaternion) =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wx2, wy2, wz2, xx2, xy2, xz2, yy2, yz2, zz2 =
            rotation.W * x2, rotation.W * y2, rotation.W * z2,
            rotation.X * x2, rotation.X * y2, rotation.X * z2,
            rotation.Y * y2, rotation.Y * z2, rotation.Z * z2

        {   X = value.X * ( 1.0 - yy2 - zz2 ) + value.Y * (       xy2 - wz2 ) + value.Z * (       xz2 + wy2 )
            Y = value.X * (       xy2 + wz2 ) + value.Y * ( 1.0 - xx2 - zz2 ) + value.Z * (       yz2 - wx2 )
            Z = value.X * (       xz2 - wy2 ) + value.Y * (       yz2 + wx2 ) + value.Z * ( 1.0 - xx2 - yy2 )
            W = 1.0   }
        
        
    let TransformVec4Q (value : Vector4) (rotation : Quaternion) =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wx2, wy2, wz2, xx2, xy2, xz2, yy2, yz2, zz2 =
            rotation.W * x2, rotation.W * y2, rotation.W * z2,
            rotation.X * x2, rotation.X * y2, rotation.X * z2,
            rotation.Y * y2, rotation.Y * z2, rotation.Z * z2

        {   X = value.X * ( 1.0 - yy2 - zz2 ) + value.Y * (        xy2 - wz2 ) + value.Z * (        xz2 + wy2 )
            Y = value.X * (        xy2 + wz2 ) + value.Y * ( 1.0 - xx2 - zz2 ) + value.Z * (        yz2 - wx2 )
            Z = value.X * (        xz2 - wy2 ) + value.Y * (        yz2 + wx2 ) + value.Z * ( 1.0 - xx2 - yy2 )
            W = value.W   }

    let Negate (v : Vector4) : Vector4 =
        {   X = -v.X ; Y = -v.Y ; Z = -v.Z ; W = -v.W   }

    let Add (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z ; W = v1.W + v2.W   }

    let Subtract (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z ; W = v1.W - v2.W   }

    let Multiply (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = v1.X * v2.X ; Y = v1.Y * v2.Y ; Z = v1.Z * v2.Z ; W = v1.W * v2.W   }
    
    let MultiplyFloat (v1 : Vector4) (v2 : float) : Vector4 =
        {   X = v1.X * v2 ; Y = v1.Y * v2 ; Z = v1.Z * v2 ; W = v1.W * v2   }

    let Divide (v1 : Vector4) (v2 : Vector4) : Vector4 =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y ; Z = v1.Z / v2.Z ; W = v1.W / v2.W   }
    
    let DivideFloat (v1 : Vector4) (v2 : float) : Vector4 =
        let invDiv = 1.0 / v2
        {   X = v1.X * invDiv ; Y = v1.Y * invDiv ; Z = v1.Z / v2 ; W = v1.W * invDiv   }

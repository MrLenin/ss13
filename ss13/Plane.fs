namespace ss13

open System
open System.Globalization

open MathTypes
open Matrix4x4

module Plane =
    let CreateFromVertices (point1 : Vector3) (point2 : Vector3) (point3 : Vector3) =
        let ax, ay, az =
            point2.X - point1.X,
            point2.Y - point1.Y,
            point2.Z - point1.Z

        let bx, by, bz =
            point3.X - point1.X,
            point3.Y - point1.Y,
            point3.Z - point1.Z

        // N=Cross(a,b)
        let nx, ny, nz =
            ay * bz - az * by,
            az * bx - ax * bz,
            ax * by - ay * bx

        // Normalize(N)
        let ls = nx * nx + ny * ny + nz * nz;
        let invNorm = 1.0 / sqrt ls

        let result : Vector3 = { X = nx * invNorm ; Y = ny * invNorm ; Z = nz * invNorm }
        { Normal = result ; D = -(result.X * point1.X + result.Y * point1.Y + result.Z * point1.Z) }

    let Normalize (value : Plane) =
        let FLT_EPSILON = 1.192092896e-07 // smallest such that 1.0+FLT_EPSILON != 1.0
        let f = value.Normal.X * value.Normal.X + value.Normal.Y * value.Normal.Y + value.Normal.Z * value.Normal.Z

        if abs (f - 1.0) < FLT_EPSILON then
            { Normal = value.Normal ; D = value.D }
        else
            let fInv = 1.0 / sqrt f
            { Normal = value.Normal * fInv ; D = value.D * fInv }


    let Transform (plane : Plane) (matrix : Matrix4x4) =
        let m = Matrix4x4.Invert matrix
        if m.IsSome then
            let m = m.Value
            let x, y, z, w =
                plane.Normal.X, plane.Normal.Y, plane.Normal.Z, plane.D
            let normal : Vector3 =
                {   X = x * m.M11 + y * m.M12 + z * m.M13 + w * m.M14
                    Y = x * m.M21 + y * m.M22 + z * m.M23 + w * m.M24
                    Z = x * m.M31 + y * m.M32 + z * m.M33 + w * m.M34   }

            { Normal = normal ; D = x * m.M41 + y * m.M42 + z * m.M43 + w * m.M44 }
        else {   Normal = { X = Double.NaN ; Y = Double.NaN ; Z = Double.NaN } ; D = Double.NaN   }


    let TransformQ (plane : Plane) (rotation : Quaternion) =
        let x2, y2, z2 =
            rotation.X + rotation.X,
            rotation.Y + rotation.Y,
            rotation.Z + rotation.Z

        let wx2, wy2, wz2, xx2, xy2, xz2, yy2, yz2, zz2 =
            rotation.W * x2, rotation.W * y2, rotation.W * z2,
            rotation.X * x2, rotation.X * y2, rotation.X * z2,
            rotation.Y * y2, rotation.Y * z2, rotation.Z * z2

        let m11, m21, m31, m12, m22, m32, m13, m23, m33 =
            1.0 - yy2 - zz2, xy2 - wz2, xz2 + wy2,
            xy2 + wz2, 1.0 - xx2 - zz2, yz2 - wx2,
            xz2 - wy2, yz2 + wx2, 1.0 - xx2 - yy2

        let x, y, z = plane.Normal.X, plane.Normal.Y, plane.Normal.Z
        let normal : Vector3 =
                {   X = x * m11 + y * m21 + z * m31
                    Y = x * m12 + y * m22 + z * m32
                    Z = x * m13 + y * m23 + z * m33   }

        {   Normal = normal ; D = plane.D   }

    let Dot (plane : Plane) (value : Vector4) =
        plane.Normal.X * value.X + 
        plane.Normal.Y * value.Y + 
        plane.Normal.Z * value.Z + 
        plane.D * value.W

    let DotCoordinate (plane : Plane) (value : Vector3) =
        plane.Normal.X * value.X + 
        plane.Normal.Y * value.Y + 
        plane.Normal.Z * value.Z + 
        plane.D

    let DotNormal (plane : Plane) (value : Vector3) =
        plane.Normal.X * value.X + 
        plane.Normal.Y * value.Y + 
        plane.Normal.Z * value.Z

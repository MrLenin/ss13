namespace ss13

open System

open MathTypes

module Quaternion =
    let Identity = { X = 0.0 ; Y = 0.0 ; Z = 0.0 ; W = 1.0 }

    let Normalize (value : Quaternion) =
        let invNorm = 1.0 / sqrt (value.X * value.X + value.Y * value.Y +
                        value.Z * value.Z + value.W * value.W)
        {   X = value.X * invNorm ; Y = value.Y * invNorm
            Z = value.Z * invNorm ; W = value.W * invNorm   }
 
    let Conjugate (value : Quaternion) =
        {   X = -value.X ; Y = -value.Y ; Z = -value.Z ; W = value.W   }

    let Inverse (value : Quaternion) =
        let invNorm = 1.0 / (value.X * value.X + value.Y * value.Y + value.Z * value.Z + value.W * value.W)
        {   X = -value.X * invNorm ; Y = -value.Y * invNorm
            Z = -value.Z * invNorm ; W = value.W * invNorm   }

    let CreateFromAxisAngle (axis : Vector3) (angle : float) =
        let halfAngle = angle * 0.5
        let s = sin halfAngle
        let c = cos halfAngle
        {   X = axis.X * s ; Y = axis.Y * s
            Z = axis.Z * s ; W = c   }

    let CreateFromYawPitchRoll yaw pitch roll =
        let halfRoll, halfPitch, halfYaw =
            roll * 0.5, pitch * 0.5, yaw * 0.5

        let sr, cr, sp, cp, sy, cy =
            sin halfRoll, cos halfRoll,
            sin halfPitch, cos halfPitch,
            sin halfYaw, cos halfYaw

        {   X = cy * sp * cr + sy * cp * sr
            Y = sy * cp * cr - cy * sp * sr
            Z = cy * cp * sr - sy * sp * cr
            W = cy * cp * cr + sy * sp * sr   }

    let CreateFromRotationMatrix (matrix : Matrix4x4) =
        let trace = matrix.M11 + matrix.M22 + matrix.M33
        if trace > 0.0 then
            let s = sqrt (trace + 1.0)
            let t = 0.5 / s
            {   W = s * 0.5
                X = (matrix.M23 - matrix.M32) * t
                Y = (matrix.M31 - matrix.M13) * t
                Z = (matrix.M12 - matrix.M21) * t   }
        else
            if matrix.M11 >= matrix.M22 && matrix.M11 >= matrix.M33 then
                let s = sqrt (1.0 + matrix.M11 - matrix.M22 - matrix.M33)
                let invS = 0.5 / s
                {   X = 0.5 * s
                    Y = (matrix.M12 + matrix.M21) * invS
                    Z = (matrix.M13 + matrix.M31) * invS
                    W = (matrix.M23 - matrix.M32) * invS   }
            else if matrix.M22 > matrix.M33 then
                let s = sqrt (1.0 + matrix.M22 - matrix.M11 - matrix.M33)
                let invS = 0.5 / s
                {   X = (matrix.M21 + matrix.M12) * invS
                    Y = 0.5 * s
                    Z = (matrix.M32 + matrix.M23) * invS
                    W = (matrix.M31 - matrix.M13) * invS   }
            else
                let s = sqrt (1.0 + matrix.M33 - matrix.M11 - matrix.M22)
                let invS = 0.5 / s
                {   X = (matrix.M31 + matrix.M13) * invS
                    Y = (matrix.M32 + matrix.M23) * invS
                    Z = 0.5 * s
                    W = (matrix.M12 - matrix.M21) * invS   }

    let Dot (quaternion1 : Quaternion) (quaternion2 : Quaternion) =
        quaternion1.X * quaternion2.X + 
        quaternion1.Y * quaternion2.Y + 
        quaternion1.Z * quaternion2.Z + 
        quaternion1.W * quaternion2.W

    let Slerp (quaternion1 : Quaternion) (quaternion2 : Quaternion) (amount : float) =
        let epsilon = 1e-6
        let t = amount
        let cosOmega =
            quaternion1.X * quaternion2.X + quaternion1.Y * quaternion2.Y +
            quaternion1.Z * quaternion2.Z + quaternion1.W * quaternion2.W
        let flip, cosOmega =
            if cosOmega < 0.0 then
                true, -cosOmega
                else false, cosOmega
        let s1, s2 =
            if cosOmega > 1.0 - epsilon then
                // Too close, do straight linear interpolation.
                1.0 - t,
                if flip then -t else t
            else
                let omega = acos cosOmega
                let invSinOmega = 1.0 / sin omega

                sin ((1.0 - t) * omega) * invSinOmega, 
                if flip then -sin (t * omega) * invSinOmega
                else sin (t * omega) * invSinOmega

        {   X = s1 * quaternion1.X + s2 * quaternion2.X
            Y = s1 * quaternion1.Y + s2 * quaternion2.Y
            Z = s1 * quaternion1.Z + s2 * quaternion2.Z
            W = s1 * quaternion1.W + s2 * quaternion2.W   }

    let Lerp (quaternion1 : Quaternion, quaternion2 : Quaternion) (amount : float) =
        let t, t1 = amount, 1.0 - amount

        let dot = quaternion1.X * quaternion2.X + quaternion1.Y * quaternion2.Y +
                  quaternion1.Z * quaternion2.Z + quaternion1.W * quaternion2.W

        let r =
            if dot >= 0.0 then
                {   X = t1 * quaternion1.X + t * quaternion2.X
                    Y = t1 * quaternion1.Y + t * quaternion2.Y
                    Z = t1 * quaternion1.Z + t * quaternion2.Z
                    W = t1 * quaternion1.W + t * quaternion2.W   }
            else
                {   X = t1 * quaternion1.X - t * quaternion2.X
                    Y = t1 * quaternion1.Y - t * quaternion2.Y
                    Z = t1 * quaternion1.Z - t * quaternion2.Z
                    W = t1 * quaternion1.W - t * quaternion2.W   }

        // Normalize it.
        let ls = r.X * r.X + r.Y * r.Y + r.Z * r.Z + r.W * r.W
        let invNorm = 1.0 / sqrt ls
        {   X = r.X * invNorm ; Y = r.Y * invNorm
            Z = r.Z * invNorm ; W = r.W * invNorm   }

    let Concatenate (value1 : Quaternion) (value2 : Quaternion) =
        // Concatenate rotation is actually q2 * q1 instead of q1 * q2.
        // So that's why value2 goes q1 and value1 goes q2.
        let q1x, q1y, q1z, q1w,
            q2x, q2y, q2z, q2w =
                value2.X, value2.Y, value2.Z, value2.W,
                value1.X, value1.Y, value1.Z, value1.W

        // cross(av, bv)
        let cx, cy, cz =
            q1y * q2z - q1z * q2y,
            q1z * q2x - q1x * q2z,
            q1x * q2y - q1y * q2x

        let dot = q1x * q2x + q1y * q2y + q1z * q2z
        {   X = q1x * q2w + q2x * q1w + cx
            Y = q1y * q2w + q2y * q1w + cy
            Z = q1z * q2w + q2z * q1w + cz
            W = q1w * q2w - dot   }

    let Negate (value : Quaternion) =
        {   X = -value.X ; Y = -value.Y
            Z = -value.Z ; W = -value.W   }

    let Add (value1 : Quaternion) (value2 : Quaternion) =
        {   X = value1.X + value2.X
            Y = value1.Y + value2.Y
            Z = value1.Z + value2.Z
            W = value1.W + value2.W   }

    let Subtract (value1 : Quaternion) (value2 : Quaternion) =
        {   X = value1.X - value2.X
            Y = value1.Y - value2.Y
            Z = value1.Z - value2.Z
            W = value1.W - value2.W   }

    let Multiply (value1 : Quaternion) (value2 : Quaternion) =
        let q1x, q1y, q1z, q1w,
            q2x, q2y, q2z, q2w  =
                value1.X, value1.Y, value1.Z, value1.W,
                value2.X, value2.Y, value2.Z, value2.W

        // cross(av, bv)
        let cx, cy, cz =
            q1y * q2z - q1z * q2y,
            q1z * q2x - q1x * q2z,
            q1x * q2y - q1y * q2x

        let dot = q1x * q2x + q1y * q2y + q1z * q2z

        {   X = q1x * q2w + q2x * q1w + cx
            Y = q1y * q2w + q2y * q1w + cy
            Z = q1z * q2w + q2z * q1w + cz
            W = q1w * q2w - dot   }

    let MultiplyFloat (value1 : Quaternion) (value2 : float) =
        {   X = value1.X * value2
            Y = value1.Y * value2
            Z = value1.Z * value2
            W = value1.W * value2   }

    let Divide (value1 : Quaternion) (value2 : Quaternion) =
        let q1x, q1y, q1z, q1w  =
                value1.X, value1.Y, value1.Z, value1.W

        //-------------------------------------
        // Inverse part.
        let ls = value2.X * value2.X + value2.Y * value2.Y +
                 value2.Z * value2.Z + value2.W * value2.W
        let invNorm = 1.0 / ls

        let q2x, q2y, q2z, q2w =
            -value2.X * invNorm,
            -value2.Y * invNorm,
            -value2.Z * invNorm,
             value2.W * invNorm

        //-------------------------------------
        // Multiply part.

        // cross(av, bv)
        let cx, cy, cz =
            q1y * q2z - q1z * q2y,
            q1z * q2x - q1x * q2z,
            q1x * q2y - q1y * q2x;

        let dot = q1x * q2x + q1y * q2y + q1z * q2z

        {   X = q1x * q2w + q2x * q1w + cx
            Y = q1y * q2w + q2y * q1w + cy
            Z = q1z * q2w + q2z * q1w + cz
            W = q1w * q2w - dot   }
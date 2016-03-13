namespace ss13

open System
open System.Globalization

open MathTypes
open Quaternion

// TODO: Need to convert this to use mathematically correct column vectors in place of row vectors

module Matrix3x2 =
    let Identity : Matrix3x2 =
        {   M11 = 1.0 ; M12 = 0.0
            M21 = 0.0 ; M22 = 1.0
            M31 = 0.0 ; M32 = 0.0   }

    let CreateTranslation (pos : Vector2) : Matrix3x2 =
        {   M11 = 1.0   ; M12 = 0.0
            M21 = 0.0   ; M22 = 1.0
            M31 = pos.X ; M32 = pos.Y   }

    let CreateScaleFloat2Vec xScale yScale (centerPoint : Vector2) : Matrix3x2 =
        let tx, ty =
            centerPoint.X * (1.0 - xScale),
            centerPoint.Y * (1.0 - yScale)
        {   M11 = xScale ; M12 = 0.0
            M21 = 0.0    ; M22 = yScale
            M31 = tx     ; M32 = ty   }

    let CreateScaleVec (scales : Vector2) : Matrix3x2 =
        {   M11 = scales.X ; M12 = 0.0
            M21 = 0.0      ; M22 = scales.Y
            M31 = 0.0      ; M32 = 0.0   }

    let CreateScaleVec2 (scales : Vector2) (centerPoint : Vector2) : Matrix3x2 =
        let tx, ty =
            centerPoint.X * (1.0 - scales.X),
            centerPoint.Y * (1.0 - scales.Y)
        {   M11 = scales.X ; M12 = 0.0
            M21 = 0.0    ; M22 = scales.Y
            M31 = tx     ; M32 = ty   }

    let CreateScaleFloat (scale : float) : Matrix3x2 =
        {   M11 = scale ; M12 = 0.0
            M21 = 0.0   ; M22 = scale
            M31 = 0.0   ; M32 = 0.0   }

    let CreateScaleFloatVec (scale : float) (centerPoint : Vector2) : Matrix3x2 =
        let tx, ty =
            centerPoint.X * (1.0 - scale),
            centerPoint.Y * (1.0 - scale)
        {   M11 = scale ; M12 = 0.0
            M21 = 0.0    ; M22 = scale
            M31 = tx     ; M32 = ty   }

    let CreateSkew (radiansX : float) (radiansY : float) : Matrix3x2 =
        let xTan, yTan =
            tan radiansX,
            tan radiansY
        {   M11 = 1.0  ; M12 = yTan
            M21 = xTan ; M22 = 1.0
            M31 = 0.0  ; M32 = 0.0   }

    let CreateSkewVec (radiansX : float) (radiansY : float) (centerPoint : Vector2) : Matrix3x2 =
        let xTan, yTan =
            tan radiansX,
            tan radiansY
        let tx, ty =
            -centerPoint.Y * xTan,
            -centerPoint.X * yTan
        {   M11 = 1.0  ; M12 = yTan
            M21 = xTan ; M22 = 1.0
            M31 = tx   ; M32 = ty   }

    let CreateRotation (radians : float) : Matrix3x2 =
        let epsilon = 0.001 * Math.PI / 180.0  
        let radians = Math.IEEERemainder (radians, Math.PI * 2.0)
        let c, s =
            // Exact case for zero rotation.
            if radians > -epsilon && radians < epsilon then
                1.0, 0.0

            // Exact case for 90 degree rotation.
            else if radians > Math.PI / 2.0 - epsilon && radians < Math.PI / 2.0 + epsilon then
                0.0, 1.0

            // Exact case for 180 degree rotation.
            else if radians < -Math.PI + epsilon || radians > Math.PI - epsilon then
                -1.0, 0.0

            // Exact case for 270 degree rotation.
            else if radians > -Math.PI / 2.0 - epsilon && radians < -Math.PI / 2.0 + epsilon then
                0.0, -1.0

            // Arbitrary rotation.
            else cos radians, sin radians
        {   M11 = c   ; M12 = s
            M21 = -s  ; M22 = c
            M31 = 0.0 ; M32 = 0.0   }

    let CreateRotationVec (radians : float) (centerPoint) : Matrix3x2 =
        let epsilon = 0.001 * Math.PI / 180.0 
        let radians = Math.IEEERemainder  (radians, Math.PI * 2.0)
        let c, s =
            // Exact case for zero rotation.
            if radians > -epsilon && radians < epsilon then
                1.0, 0.0

            // Exact case for 90 degree rotation.
            else if radians > Math.PI / 2.0 - epsilon && radians < Math.PI / 2.0 + epsilon then
                0.0, 1.0
            
            // Exact case for 180 degree rotation.
            else if radians < -Math.PI + epsilon || radians > Math.PI - epsilon then
                -1.0, 0.0
            
            // Exact case for 270 degree rotation.
            else if radians > -Math.PI / 2.0 - epsilon && radians < -Math.PI / 2.0 + epsilon then
                0.0, -1.0
            
            // Arbitrary rotation.
            else cos radians, sin radians

        let x, y =
            centerPoint.X * (1.0 - c) + centerPoint.Y * s,
            centerPoint.Y * (1.0 - c) - centerPoint.X * s
        {   M11 = c  ; M12 = s
            M21 = -s ; M22 = c
            M31 = x  ; M32 = y   }

    let Invert (matrix : Matrix3x2) : Matrix3x2 option =
        let det = (matrix.M11 * matrix.M22) - (matrix.M21 * matrix.M12)

        if abs det < Double.Epsilon then
            None
        else
            let invDet = 1.0 / det
            Some
                {   M11 = matrix.M22 * invDet  ; M12 = -matrix.M12 * invDet
                    M21 = -matrix.M21 * invDet ; M22 = matrix.M11 * invDet
                    M31 = (matrix.M21 * matrix.M32 - matrix.M31 * matrix.M22) * invDet
                    M32 = (matrix.M31 * matrix.M12 - matrix.M11 * matrix.M32) * invDet   }

    let Lerp (matrix1 : Matrix3x2) (matrix2 : Matrix3x2) (amount : float) : Matrix3x2 =
        {   M11 = matrix1.M11 + (matrix2.M11 - matrix1.M11) * amount
            M12 = matrix1.M12 + (matrix2.M12 - matrix1.M12) * amount
            M21 = matrix1.M21 + (matrix2.M21 - matrix1.M21) * amount
            M22 = matrix1.M22 + (matrix2.M22 - matrix1.M22) * amount
            M31 = matrix1.M31 + (matrix2.M31 - matrix1.M31) * amount
            M32 = matrix1.M32 + (matrix2.M32 - matrix1.M32) * amount   }

    let Negate (value : Matrix3x2) : Matrix3x2 =
        {   M11 = -value.M11 ; M12 = -value.M12
            M21 = -value.M21 ; M22 = -value.M22
            M31 = -value.M31 ; M32 = -value.M32   }

    let Add (value1 : Matrix3x2) (value2 : Matrix3x2) : Matrix3x2 =
        {   M11 = value1.M11 + value2.M11 ; M12 = value1.M12 + value2.M12
            M21 = value1.M21 + value2.M21 ; M22 = value1.M22 + value2.M22
            M31 = value1.M31 + value2.M31 ; M32 = value1.M32 + value2.M32   }

    let Subtract (value1 : Matrix3x2) (value2 : Matrix3x2) : Matrix3x2 =
        {   M11 = value1.M11 - value2.M11 ; M12 = value1.M12 - value2.M12
            M21 = value1.M21 - value2.M21 ; M22 = value1.M22 - value2.M22
            M31 = value1.M31 - value2.M31 ; M32 = value1.M32 - value2.M32   }

    let Multiply (value1 : Matrix3x2) (value2 : Matrix3x2) : Matrix3x2 =
        {   M11 = value1.M11 * value2.M11 + value1.M12 * value2.M21
            M12 = value1.M11 * value2.M12 + value1.M12 * value2.M22
            M21 = value1.M21 * value2.M11 + value1.M22 * value2.M21
            M22 = value1.M21 * value2.M12 + value1.M22 * value2.M22
            M31 = value1.M31 * value2.M11 + value1.M32 * value2.M21 + value2.M31
            M32 = value1.M31 * value2.M12 + value1.M32 * value2.M22 + value2.M32   }
    
    let MultiplyFloat (value1 : Matrix3x2) (value2 : float) : Matrix3x2 =
        {   M11 = value1.M11 * value2 ; M12 = value1.M12 * value2
            M21 = value1.M21 * value2 ; M22 = value1.M22 * value2
            M31 = value1.M31 * value2 ; M32 = value1.M32 * value2   }

module Matrix4x4 =
    let private NormalizePlane (value : Plane) =
        let FLT_EPSILON = 1.192092896e-07 // smallest such that 1.0+FLT_EPSILON != 1.0
        let f = value.Normal.X * value.Normal.X + value.Normal.Y * value.Normal.Y + value.Normal.Z * value.Normal.Z

        if abs (f - 1.0) < FLT_EPSILON then
            { Normal = value.Normal ; D = value.D }
        else
            let fInv = 1.0 / sqrt f
            { Normal = value.Normal * fInv ; D = value.D * fInv }

    let Identity : Matrix4x4 =
        {   M11 = 1.0 ; M12 = 0.0 ; M13 = 0.0 ; M14 = 0.0
            M21 = 0.0 ; M22 = 1.0 ; M23 = 0.0 ; M24 = 0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = 1.0 ; M34 = 0.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = 0.0 ; M44 = 1.0   }

    let CreateBillboard (objectPos : Vector3) (cameraPos : Vector3) (cameraUp : Vector3) (cameraForward : Vector3) =
        let epsilon = 1e-4
        let zaxis : Vector3 =
            {   X = objectPos.X - cameraPos.X
                Y = objectPos.Y - cameraPos.Y
                Z = objectPos.Z - cameraPos.Z   }

        let norm = zaxis.LengthSquared
        let zaxis =
            if norm < epsilon then -cameraForward
            else Vector3.MultiplyFloat zaxis (1.0 / sqrt norm)

        let xaxis = Vector3.Normalize (Vector3.Cross cameraUp zaxis)

        let yaxis = Vector3.Cross zaxis xaxis

        {   M11 = xaxis.X     ; M12 = xaxis.Y     ; M13 = xaxis.Z     ; M14 = 0.0
            M21 = yaxis.X     ; M22 = yaxis.Y     ; M23 = yaxis.Z     ; M24 = 0.0
            M31 = zaxis.X     ; M32 = zaxis.Y     ; M33 = zaxis.Z     ; M34 = 0.0
            M41 = objectPos.X ; M42 = objectPos.Y ; M43 = objectPos.Z ; M44 = 1.0   }

    let CreateConstrainedBillboard (objectPos : Vector3) (cameraPos : Vector3)
            (rotateAxis : Vector3) (cameraForward : Vector3) (objectForward : Vector3) : Matrix4x4 =
        let epsilon = 1e-4
        let minAngle = 1.0 - (0.1 * (Math.PI / 180.0)) // 0.1 degrees

        // Treat the case when object and camera positions are too close.
        let faceDir : Vector3 =
            {   X = objectPos.X - cameraPos.X
                Y = objectPos.Y - cameraPos.Y
                Z = objectPos.Z - cameraPos.Z   }

        let norm = faceDir.LengthSquared
        let faceDir =
            if norm < epsilon then -cameraForward
            else Vector3.MultiplyFloat faceDir (1.0 / sqrt norm)

        let yaxis = rotateAxis

        // Treat the case when angle between faceDir and rotateAxis is too close to 0.
        let dot = Vector3.Dot rotateAxis faceDir

        let xaxis, zaxis =
            if abs dot > minAngle then
                // Make sure passed values are useful for compute.
                let dot = Vector3.Dot rotateAxis objectForward
                let zaxis : Vector3 =
                    if abs dot > minAngle then
                        if abs rotateAxis.Z > minAngle then
                             { X = 1.0 ; Y = 0.0 ; Z =  0.0 }
                        else { X = 0.0 ; Y = 0.0 ; Z = -1.0 }
                    else objectForward

                let xaxis = Vector3.Normalize (Vector3.Cross rotateAxis zaxis)
                xaxis, Vector3.Normalize (Vector3.Cross xaxis rotateAxis)
            else
                let xaxis = Vector3.Normalize (Vector3.Cross rotateAxis faceDir)
                xaxis, Vector3.Normalize (Vector3.Cross xaxis yaxis)

        {   M11 = xaxis.X     ; M12 = xaxis.Y     ; M13 = xaxis.Z     ; M14 = 0.0
            M21 = yaxis.X     ; M22 = yaxis.Y     ; M23 = yaxis.Z     ; M24 = 0.0
            M31 = zaxis.X     ; M32 = zaxis.Y     ; M33 = zaxis.Z     ; M34 = 0.0
            M41 = objectPos.X ; M42 = objectPos.Y ; M43 = objectPos.Z ; M44 = 1.0   }

    let CreatePerspective width height near far =
        let m11 = 2.0 * near / width
        let m22 = 2.0 * near / height
        let m33 = far / (near - far)
        let m43 = near * far / (near - far)
        {   M11 = m11 ; M12 = 0.0 ; M13 = 0.0 ; M14 =  0.0
            M21 = 0.0 ; M22 = m22 ; M23 = 0.0 ; M24 =  0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = m33 ; M34 = -1.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = m43 ; M44 =  1.0   }

    let CreatePerspectiveOffCenter left right bottom top near far =
        let m11 = 2.0 * near / (right - left)
        let m22 = 2.0 * near / (top - bottom)
        let m31 = (left + right) / (right - left)
        let m32 = (top + bottom) / (top - bottom)
        let m33 = far / (near - far)
        let m43 = near * far / (near - far)
        {   M11 = m11 ; M12 = 0.0 ; M13 = 0.0 ; M14 =  0.0
            M21 = 0.0 ; M22 = m22 ; M23 = 0.0 ; M24 =  0.0
            M31 = m31 ; M32 = m32 ; M33 = m33 ; M34 = -1.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = m43 ; M44 =  1.0   }

    let CreatePerspectiveFieldOfView fov aspect near far =
        let m22 = 1.0 / tan fov * 0.5
        let m11 = m22 / aspect
        let m33 = far / (near - far)
        let m43 = near * far / (near - far)
        {   M11 = m11 ; M12 = 0.0 ; M13 = 0.0 ; M14 =  0.0
            M21 = 0.0 ; M22 = m22 ; M23 = 0.0 ; M24 =  0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = m33 ; M34 = -1.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = m43 ; M44 =  0.0   }

    let CreateOrthographic width height near far =
        let m11 = 2.0 / width
        let m22 = 2.0 / height
        let m33 = 1.0 / (near - far)
        let m43 = near / (near - far)
        {   M11 = m11 ; M12 = 0.0 ; M13 = 0.0 ; M14 = 0.0
            M21 = 0.0 ; M22 = m22 ; M23 = 0.0 ; M24 = 0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = m33 ; M34 = 0.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = m43 ; M44 = 1.0   }

    let CreateOrthographicOffCenter left right bottom top near far =
        let m11 = 2.0 / (right - left)
        let m22 = 2.0 / (top - bottom)
        let m33 = 1.0 / (near - far)
        let m41 = (left + right) / (left - right)
        let m42 = (top + bottom) / (bottom - top)
        let m43 = near / (near - far)
        {   M11 = m11 ; M12 = 0.0 ; M13 = 0.0 ; M14 = 0.0
            M21 = 0.0 ; M22 = m22 ; M23 = 0.0 ; M24 = 0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = m33 ; M34 = 0.0
            M41 = m41 ; M42 = m42 ; M43 = m43 ; M44 = 1.0   }

    let CreateScaleFloat3 (xScale, yScale, zScale) =
        {   M11 = xScale ; M12 = 0.0    ; M13 = 0.0    ; M14 = 0.0
            M21 = 0.0    ; M22 = yScale ; M23 = 0.0    ; M24 = 0.0
            M31 = 0.0    ; M32 = 0.0    ; M33 = zScale ; M34 = 0.0
            M41 = 0.0    ; M42 = 0.0    ; M43 = 0.0    ; M44 = 1.0   }

    let CreateScaleFloat scale =
        CreateScaleFloat3 (scale, scale, scale)

    let CreateScaleFloat3Vec (xScale : float, yScale : float, zScale : float, centerPoint : Vector3) =
        let tx, ty, tz =
            centerPoint.X * (1.0 - xScale),
            centerPoint.Y * (1.0 - yScale),
            centerPoint.Z * (1.0 - zScale)
        {   M11 = xScale ; M12 = 0.0    ; M13 = 0.0    ; M14 = 0.0
            M21 = 0.0    ; M22 = yScale ; M23 = 0.0    ; M24 = 0.0
            M31 = 0.0    ; M32 = 0.0    ; M33 = zScale ; M34 = 0.0
            M41 = tx     ; M42 = ty     ; M43 = tz     ; M44 = 1.0   }

    let CreateScaleVec (scales : Vector3) =
        {   M11 = scales.X ; M12 = 0.0      ; M13 = 0.0      ; M14 = 0.0
            M21 = 0.0      ; M22 = scales.Y ; M23 = 0.0      ; M24 = 0.0
            M31 = 0.0      ; M32 = 0.0      ; M33 = scales.Z ; M34 = 0.0
            M41 = 0.0      ; M42 = 0.0      ; M43 = 0.0      ; M44 = 1.0   }

    let CreateScaleVec2 (scales : Vector3, centerPoint : Vector3) =
        let tx, ty, tz =
            centerPoint.X * (1.0 - scales.X),
            centerPoint.Y * (1.0 - scales.Y),
            centerPoint.Z * (1.0 - scales.Z)
        {   M11 = scales.X ; M12 = 0.0      ; M13 = 0.0      ; M14 = 0.0
            M21 = 0.0      ; M22 = scales.Y ; M23 = 0.0      ; M24 = 0.0
            M31 = 0.0      ; M32 = 0.0      ; M33 = scales.Z ; M34 = 0.0
            M41 = tx       ; M42 = ty       ; M43 = tz       ; M44 = 1.0   }

    let CreateScaleFloatVec (scale : float, centerPoint : Vector3) =
        let tx, ty, tz =
            centerPoint.X * (1.0 - scale),
            centerPoint.Y * (1.0 - scale),
            centerPoint.Z * (1.0 - scale)
        {   M11 = scale ; M12 = 0.0   ; M13 = 0.0   ; M14 = 0.0
            M21 = 0.0   ; M22 = scale ; M23 = 0.0   ; M24 = 0.0
            M31 = 0.0   ; M32 = 0.0   ; M33 = scale ; M34 = 0.0
            M41 = tx    ; M42 = ty    ; M43 = tz    ; M44 = 1.0   }

    let CreateTranslation (pos : Vector3) =
        {   M11 = 1.0   ; M12 = 0.0   ; M13 = 0.0   ; M14 = 0.0
            M21 = 0.0   ; M22 = 1.0   ; M23 = 0.0   ; M24 = 0.0
            M31 = 0.0   ; M32 = 0.0   ; M33 = 1.0   ; M34 = 0.0
            M41 = pos.X ; M42 = pos.Y ; M43 = pos.Z ; M44 = 1.0   }

    let CreateTranslationFloat3 (xPos, yPos, zPos) =
        {   M11 = 1.0  ; M12 = 0.0  ; M13 = 0.0  ; M14 = 0.0
            M21 = 0.0  ; M22 = 1.0  ; M23 = 0.0  ; M24 = 0.0
            M31 = 0.0  ; M32 = 0.0  ; M33 = 1.0  ; M34 = 0.0
            M41 = xPos ; M42 = yPos ; M43 = zPos ; M44 = 1.0   }

    let CreateRotationX radians =
        let c = cos radians
        let s = sin radians
        {   M11 = 1.0 ; M12 = 0.0 ; M13 = 0.0 ; M14 = 0.0
            M21 = 0.0 ; M22 = c   ; M23 = s   ; M24 = 0.0
            M31 = 0.0 ; M32 = -s  ; M33 = c   ; M34 = 0.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = 0.0 ; M44 = 1.0   }

    let CreateRotationXVec (radians, centerPoint : Vector3) =
        let c = cos radians
        let s = sin radians
        let y = centerPoint.Y * (1.0 - c) + centerPoint.Z * s
        let z = centerPoint.Z * (1.0 - c) - centerPoint.Y * s
        {   M11 = 1.0 ; M12 = 0.0 ; M13 = 0.0 ; M14 = 0.0
            M21 = 0.0 ; M22 = c   ; M23 = s   ; M24 = 0.0
            M31 = 0.0 ; M32 = -s  ; M33 = c   ; M34 = 0.0
            M41 = 0.0 ; M42 = y   ; M43 = z   ; M44 = 1.0   }

    let CreateRotationY radians =
        let c = cos radians
        let s = sin radians
        {   M11 = c   ; M12 = 0.0 ; M13 = -s  ; M14 = 0.0
            M21 = 0.0 ; M22 = 1.0 ; M23 = 0.0 ; M24 = 0.0
            M31 = s   ; M32 = 0.0 ; M33 = c   ; M34 = 0.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = 0.0 ; M44 = 1.0   }

    let CreateRotationYVec (radians, centerPoint : Vector3) =
        let c = cos radians
        let s = sin radians
        let x = centerPoint.X * (1.0 - c) - centerPoint.Z * s
        let z = centerPoint.Z * (1.0 - c) + centerPoint.X * s
        {   M11 = c   ; M12 = 0.0 ; M13 = -s  ; M14 = 0.0
            M21 = 0.0 ; M22 = 1.0 ; M23 = 0.0 ; M24 = 0.0
            M31 = s   ; M32 = 0.0 ; M33 = c   ; M34 = 0.0
            M41 = x   ; M42 = 0.0 ; M43 = z   ; M44 = 1.0   }

    let CreateRotationZ radians =
        let c = cos radians
        let s = sin radians
        {   M11 = c   ; M12 = s   ; M13 = 0.0 ; M14 = 0.0
            M21 = -s  ; M22 = c   ; M23 = 0.0 ; M24 = 0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = 1.0 ; M34 = 0.0
            M41 = 0.0 ; M42 = 0.0 ; M43 = 0.0 ; M44 = 1.0   }

    let CreateRotationZVec (radians, centerPoint : Vector3) =
        let c = cos radians
        let s = sin radians
        let x = centerPoint.X * (1.0 - c) + centerPoint.Y * s
        let y = centerPoint.Y * (1.0 - c) - centerPoint.X * s
        {   M11 = c   ; M12 = s   ; M13 = 0.0 ; M14 = 0.0
            M21 = -s  ; M22 = c   ; M23 = 0.0 ; M24 = 0.0
            M31 = 0.0 ; M32 = 0.0 ; M33 = 1.0 ; M34 = 0.0
            M41 = x   ; M42 = y   ; M43 = 0.0 ; M44 = 1.0   }

    let CreateFromAxisAngle (axis : Vector3) (angle : float) =
        let x, y, z = axis.X, axis.Y, axis.Z
        let sa, ca = sin angle, cos angle
        let xx, yy, zz = x * x, y * y, z * z
        let xy, xz, yz = x * y, x * z, y * z
        {   M11 = xx + ca * (1.0 - xx)  ; M12 = xy - ca * xy + sa * z ; M13 = xz - ca * xz - sa * y ; M14 = 0.0
            M21 = xy - ca * xy - sa * z ; M22 = yy + ca * (1.0 - yy)  ; M23 = yz - ca * yz + sa * x ; M24 = 0.0
            M31 = xz - ca * xz + sa * y ; M32 = yz - ca * yz - sa * x ; M33 = zz + ca * (1.0 - zz)  ; M34 = 0.0
            M41 = 0.0                   ; M42 = 0.0                   ; M43 = 0.0                   ; M44 = 1.0   }

    let CreateLookAt (cameraPos : Vector3) (cameraTarget : Vector3) (cameraUpVector : Vector3) =
        let zaxis = Vector3.Normalize (cameraPos - cameraTarget)
        let xaxis = Vector3.Normalize (Vector3.Cross cameraUpVector zaxis)
        let yaxis = Vector3.Cross zaxis xaxis
        let tx, ty, tz =
            -Vector3.Dot xaxis cameraPos,
            -Vector3.Dot yaxis cameraPos,
            -Vector3.Dot zaxis cameraPos
        {   M11 = xaxis.X ; M12 = yaxis.X ; M13 = zaxis.X ; M14 = 0.0
            M21 = xaxis.Y ; M22 = yaxis.Y ; M23 = zaxis.Y ; M24 = 0.0
            M31 = xaxis.Z ; M32 = yaxis.Z ; M33 = zaxis.Z ; M34 = 0.0
            M41 = tx      ; M42 = ty      ; M43 = tz      ; M44 = 1.0   }

    let CreateWorld (pos : Vector3) (forward : Vector3) (up : Vector3) =
        let zaxis = Vector3.Normalize -forward
        let xaxis = Vector3.Normalize (Vector3.Cross up zaxis)
        let yaxis = Vector3.Cross zaxis xaxis
        {   M11 = xaxis.X ; M12 = yaxis.X ; M13 = zaxis.X ; M14 = 0.0
            M21 = xaxis.Y ; M22 = yaxis.Y ; M23 = zaxis.Y ; M24 = 0.0
            M31 = xaxis.Z ; M32 = yaxis.Z ; M33 = zaxis.Z ; M34 = 0.0
            M41 = pos.X   ; M42 = pos.Y   ; M43 = pos.Z   ; M44 = 1.0   }

    let CreateFromQuaternion (quaternion : Quaternion) : Matrix4x4 =
        let xx, yy, zz =
            quaternion.X * quaternion.X,
            quaternion.Y * quaternion.Y,
            quaternion.Z * quaternion.Z

        let xy, wz, xz, wy, yz, wx =
            quaternion.X * quaternion.Y, quaternion.Z * quaternion.W, quaternion.Z * quaternion.X,
            quaternion.Y * quaternion.W, quaternion.Y * quaternion.Z, quaternion.X * quaternion.W

        {   M11 = 1.0 - 2.0 * (yy + zz) ; M12 = 2.0 * (xy + wz)       ; M13 = 2.0 * (xz - wy)       ; M14 = 0.0
            M21 = 2.0 * (xy - wz)       ; M22 = 1.0 - 2.0 * (zz + xx) ; M23 = 2.0 * (yz + wx)       ; M24 = 0.0
            M31 = 2.0 * (xz + wy)       ; M32 = 2.0 * (yz - wx)       ; M33 = 1.0 - 2.0 * (yy + xx) ; M34 = 0.0
            M41 = 0.0                   ; M42 = 0.0                   ; M43 = 0.0                   ; M44 = 1.0   }

    let CreateFromYawPitchRoll yaw pitch roll =
        let q = Quaternion.CreateFromYawPitchRoll yaw pitch roll
        CreateFromQuaternion q

    let CreateShadow (lightDirection : Vector3) (plane : Plane) =
        let p = NormalizePlane plane
        let dot = p.Normal.X * lightDirection.X + p.Normal.Y * lightDirection.Y + p.Normal.Z * lightDirection.Z;
        let a, b, c, d =
            -p.Normal.X, -p.Normal.Y, -p.Normal.Z, -p.D
        {   M11 = a * lightDirection.X + dot
            M21 = b * lightDirection.X
            M31 = c * lightDirection.X
            M41 = d * lightDirection.X

            M12 = a * lightDirection.Y
            M22 = b * lightDirection.Y + dot
            M32 = c * lightDirection.Y
            M42 = d * lightDirection.Y

            M13 = a * lightDirection.Z
            M23 = b * lightDirection.Z
            M33 = c * lightDirection.Z + dot
            M43 = d * lightDirection.Z

            M14 = 0.0
            M24 = 0.0
            M34 = 0.0
            M44 = dot   }

    let CreateReflection (value : Plane) =
        let value = NormalizePlane value
        let a, b, c =
            value.Normal.X, value.Normal.Y, value.Normal.Z
        let fa, fb, fc =
            -2.0 * a, -2.0 * b, -2.0 * c

        {   M11 = fa * a + 1.0 ; M12 = fb * a       ; M13 = fc * a       ; M14 = 0.0
            M21 = fa * b       ; M22 = fb * b + 1.0 ; M23 = fc * b       ; M24 = 0.0
            M31 = fa * c       ; M32 = fb * c       ; M33 = fc * c + 1.0 ; M34 = 0.0
            M41 = fa * value.D ; M42 = fb * value.D ; M43 = fc * value.D ; M44 = 1.0   }

    let Invert (v : Matrix4x4) =
        let a, b, c, d,
            e, f, g, h,
            i, j, k, l,
            m, n, o, p = 
                v.M11, v.M12, v.M13, v.M14,
                v.M21, v.M22, v.M23, v.M24,
                v.M31, v.M32, v.M33, v.M34,
                v.M41, v.M42, v.M43, v.M44

        let kp_lo, jp_ln, jo_kn,
            ip_lm, io_km, in_jm =
                k * p - l * o, j * p - l * n, j * o - k * n,
                i * p - l * m, i * o - k * m, i * n - j * m

        let a11, a12, a13, a14 =
            +(f * kp_lo - g * jp_ln + h * jo_kn),
            -(e * kp_lo - g * ip_lm + h * io_km),
            +(e * jp_ln - f * ip_lm + h * in_jm),
            -(e * jo_kn - f * io_km + g * in_jm)

        let det = a * a11 + b * a12 + c * a13 + d * a14

        if (abs det < Double.Epsilon) then
            None
        else
            let invDet = 1.0 / det

            let gp_ho, fp_hn, fo_gn, ep_hm, eo_gm, en_fm,
                gl_hk, fl_hj, fk_gj, el_hi, ek_gi, ej_fi =
                    g * p - h * o, f * p - h * n, f * o - g * n, e * p - h * m, e * o - g * m, e * n - f * m,
                    g * l - h * k, f * l - h * j, f * k - g * j, e * l - h * i, e * k - g * i, e * j - f * i

            Some
                {   M11 = a11 * invDet;
                    M12 = -(b * kp_lo - c * jp_ln + d * jo_kn) * invDet
                    M13 = +(b * gp_ho - c * fp_hn + d * fo_gn) * invDet
                    M14 = -(b * gl_hk - c * fl_hj + d * fk_gj) * invDet

                    M21 = a12 * invDet;
                    M22 = +(a * kp_lo - c * ip_lm + d * io_km) * invDet
                    M23 = -(a * gp_ho - c * ep_hm + d * eo_gm) * invDet
                    M24 = +(a * gl_hk - c * el_hi + d * ek_gi) * invDet

                    M31 = a13 * invDet;
                    M32 = -(a * jp_ln - b * ip_lm + d * in_jm) * invDet
                    M33 = +(a * fp_hn - b * ep_hm + d * en_fm) * invDet
                    M34 = -(a * fl_hj - b * el_hi + d * ej_fi) * invDet

                    M41 = a14 * invDet;
                    M42 = +(a * jo_kn - b * io_km + c * in_jm) * invDet
                    M43 = -(a * fo_gn - b * eo_gm + c * en_fm) * invDet
                    M44 = +(a * fk_gj - b * ek_gi + c * ej_fi) * invDet   }

    let Transform (value : Matrix4x4) (rotation : Quaternion) =
        let x2, y2, z2 =
            rotation.X + rotation.X, rotation.Y + rotation.Y, rotation.Z + rotation.Z

        let wx2, wy2, wz2, xx2, xy2, xz2, yy2, yz2, zz2 =
            rotation.W * x2, rotation.W * y2, rotation.W * z2,
            rotation.X * x2, rotation.X * y2, rotation.X * z2,
            rotation.Y * y2, rotation.Y * z2, rotation.Z * z2

        let q11, q21, q31, q12, q22, q32, q13, q23, q33 =
            1.0 - yy2 - zz2, xy2 - wz2, xz2 + wy2,
            xy2 + wz2, 1.0 - xx2 - zz2, yz2 - wx2,
            xz2 - wy2, yz2 + wx2, 1.0 - xx2 - yy2

        {   M11 = value.M11 * q11 + value.M12 * q21 + value.M13 * q31
            M12 = value.M11 * q12 + value.M12 * q22 + value.M13 * q32
            M13 = value.M11 * q13 + value.M12 * q23 + value.M13 * q33
            M14 = value.M14
            M21 = value.M21 * q11 + value.M22 * q21 + value.M23 * q31
            M22 = value.M21 * q12 + value.M22 * q22 + value.M23 * q32
            M23 = value.M21 * q13 + value.M22 * q23 + value.M23 * q33
            M24 = value.M24
            M31 = value.M31 * q11 + value.M32 * q21 + value.M33 * q31
            M32 = value.M31 * q12 + value.M32 * q22 + value.M33 * q32
            M33 = value.M31 * q13 + value.M32 * q23 + value.M33 * q33
            M34 = value.M34
            M41 = value.M41 * q11 + value.M42 * q21 + value.M43 * q31
            M42 = value.M41 * q12 + value.M42 * q22 + value.M43 * q32
            M43 = value.M41 * q13 + value.M42 * q23 + value.M43 * q33
            M44 = value.M44   }

    let Transpose (v : Matrix4x4) =
        {    M11 = v.M11 ; M12 = v.M21 ; M13 = v.M31 ; M14 = v.M41
             M21 = v.M12 ; M22 = v.M22 ; M23 = v.M32 ; M24 = v.M42
             M31 = v.M13 ; M32 = v.M23 ; M33 = v.M33 ; M34 = v.M43
             M41 = v.M14 ; M42 = v.M24 ; M43 = v.M34 ; M44 = v.M44   }

    let Lerp (v1 : Matrix4x4) (v2 : Matrix4x4)  (a : float) =
        {   M11 = v1.M11 + (v2.M11 - v1.M11) * a ; M12 = v1.M12 + (v2.M12 - v1.M12) * a
            M13 = v1.M13 + (v2.M13 - v1.M13) * a ; M14 = v1.M14 + (v2.M14 - v1.M14) * a
            M21 = v1.M21 + (v2.M21 - v1.M21) * a ; M22 = v1.M22 + (v2.M22 - v1.M22) * a
            M23 = v1.M23 + (v2.M23 - v1.M23) * a ; M24 = v1.M24 + (v2.M24 - v1.M24) * a
            M31 = v1.M31 + (v2.M31 - v1.M31) * a ; M32 = v1.M32 + (v2.M32 - v1.M32) * a
            M33 = v1.M33 + (v2.M33 - v1.M33) * a ; M34 = v1.M34 + (v2.M34 - v1.M34) * a
            M41 = v1.M41 + (v2.M41 - v1.M41) * a ; M42 = v1.M42 + (v2.M42 - v1.M42) * a
            M43 = v1.M43 + (v2.M43 - v1.M43) * a ; M44 = v1.M44 + (v2.M44 - v1.M44) * a   }

    let Negate (v : Matrix4x4) =
        {   M11 = -v.M11 ; M12 = -v.M12 ; M13 = -v.M13 ; M14 = -v.M14
            M21 = -v.M21 ; M22 = -v.M22 ; M23 = -v.M23 ; M24 = -v.M24
            M31 = -v.M31 ; M32 = -v.M32 ; M33 = -v.M33 ; M34 = -v.M34
            M41 = -v.M41 ; M42 = -v.M42 ; M43 = -v.M43 ; M44 = -v.M44   }

    let Add (v1 : Matrix4x4) (v2 : Matrix4x4) =
        {   M11 = v1.M11 + v2.M11 ; M12 = v1.M12 + v2.M12 ; M13 = v1.M13 + v2.M13 ; M14 = v1.M14 + v2.M14
            M21 = v1.M21 + v2.M21 ; M22 = v1.M22 + v2.M22 ; M23 = v1.M23 + v2.M23 ; M24 = v1.M24 + v2.M24
            M31 = v1.M31 + v2.M31 ; M32 = v1.M32 + v2.M32 ; M33 = v1.M33 + v2.M33 ; M34 = v1.M34 + v2.M34
            M41 = v1.M41 + v2.M41 ; M42 = v1.M42 + v2.M42 ; M43 = v1.M43 + v2.M43 ; M44 = v1.M44 + v2.M44   }

    let Subtract (v1 : Matrix4x4) (v2 : Matrix4x4) =
        {   M11 = v1.M11 - v2.M11 ; M12 = v1.M12 - v2.M12 ; M13 = v1.M13 - v2.M13 ; M14 = v1.M14 - v2.M14
            M21 = v1.M21 - v2.M21 ; M22 = v1.M22 - v2.M22 ; M23 = v1.M23 - v2.M23 ; M24 = v1.M24 - v2.M24
            M31 = v1.M31 - v2.M31 ; M32 = v1.M32 - v2.M32 ; M33 = v1.M33 - v2.M33 ; M34 = v1.M34 - v2.M34
            M41 = v1.M41 - v2.M41 ; M42 = v1.M42 - v2.M42 ; M43 = v1.M43 - v2.M43 ; M44 = v1.M44 - v2.M44   }

    let Multiply (v1 : Matrix4x4, v2 : Matrix4x4) =
        {   M11 = v1.M11 * v2.M11 + v1.M12 * v2.M21 + v1.M13 * v2.M31 + v1.M14 * v2.M41
            M12 = v1.M11 * v2.M12 + v1.M12 * v2.M22 + v1.M13 * v2.M32 + v1.M14 * v2.M42
            M13 = v1.M11 * v2.M13 + v1.M12 * v2.M23 + v1.M13 * v2.M33 + v1.M14 * v2.M43
            M14 = v1.M11 * v2.M14 + v1.M12 * v2.M24 + v1.M13 * v2.M34 + v1.M14 * v2.M44
            M21 = v1.M21 * v2.M11 + v1.M22 * v2.M21 + v1.M23 * v2.M31 + v1.M24 * v2.M41
            M22 = v1.M21 * v2.M12 + v1.M22 * v2.M22 + v1.M23 * v2.M32 + v1.M24 * v2.M42
            M23 = v1.M21 * v2.M13 + v1.M22 * v2.M23 + v1.M23 * v2.M33 + v1.M24 * v2.M43
            M24 = v1.M21 * v2.M14 + v1.M22 * v2.M24 + v1.M23 * v2.M34 + v1.M24 * v2.M44
            M31 = v1.M31 * v2.M11 + v1.M32 * v2.M21 + v1.M33 * v2.M31 + v1.M34 * v2.M41
            M32 = v1.M31 * v2.M12 + v1.M32 * v2.M22 + v1.M33 * v2.M32 + v1.M34 * v2.M42
            M33 = v1.M31 * v2.M13 + v1.M32 * v2.M23 + v1.M33 * v2.M33 + v1.M34 * v2.M43
            M34 = v1.M31 * v2.M14 + v1.M32 * v2.M24 + v1.M33 * v2.M34 + v1.M34 * v2.M44
            M41 = v1.M41 * v2.M11 + v1.M42 * v2.M21 + v1.M43 * v2.M31 + v1.M44 * v2.M41
            M42 = v1.M41 * v2.M12 + v1.M42 * v2.M22 + v1.M43 * v2.M32 + v1.M44 * v2.M42
            M43 = v1.M41 * v2.M13 + v1.M42 * v2.M23 + v1.M43 * v2.M33 + v1.M44 * v2.M43
            M44 = v1.M41 * v2.M14 + v1.M42 * v2.M24 + v1.M43 * v2.M34 + v1.M44 * v2.M44   }

    let MultiplyFloat (v1 : Matrix4x4, v2 : float) =
        {   M11 = v1.M11 * v2 ; M12 = v1.M12 * v2 ; M13 = v1.M13 * v2 ; M14 = v1.M14 * v2
            M21 = v1.M21 * v2 ; M22 = v1.M22 * v2 ; M23 = v1.M23 * v2 ; M24 = v1.M24 * v2
            M31 = v1.M31 * v2 ; M32 = v1.M32 * v2 ; M33 = v1.M33 * v2 ; M34 = v1.M34 * v2
            M41 = v1.M41 * v2 ; M42 = v1.M42 * v2 ; M43 = v1.M43 * v2 ; M44 = v1.M44 * v2   }

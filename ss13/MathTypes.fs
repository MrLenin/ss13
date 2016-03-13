module ss13.MathTypes

type Vector2 =
    {   X : float
        Y : float   }
    with
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y)

    member this.LengthSquared =
        this.X * this.X + this.Y * this.Y

    static member (-) (v1 : Vector2, v2 : Vector2) =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y   }

    static member (+) (v1 : Vector2, v2 : Vector2) =
        {   X = v1.X + v2.X ; Y= v1.Y + v2.Y   }

    static member (~-) (v : Vector2) =
        {   X = -v.X ; Y = -v.Y   }

    static member (*) (v1 : Vector2, v2 : Vector2) =
        {   X = v1.X * v2.X ; Y = v1.X * v2.Y   }

    static member (*) (v : Vector2, a) =
        {   X = v.X * a ; Y = v.Y * a   }

    static member (*) (a, v: Vector2) =
        {   X = v.X * a ; Y = v.Y * a   }

    static member (/) (v1 : Vector2, v2 : Vector2) =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y   }

    static member (/) (v : Vector2, a) =
        let invDiv = 1.0 / a
        {   X = v.X * invDiv ; Y = v.Y * invDiv   }

type Vector3 =
    {   X : float
        Y : float
        Z : float   }
    with
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z)

    member this.LengthSquared =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z

    static member (-) (v1 : Vector3, v2 : Vector3) =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z   }

    static member (+) (v1 : Vector3, v2 : Vector3) =
        {   X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z   }

    static member (~-) (v : Vector3) =
        {   X = -v.X ; Y = -v.Y ; Z = -v.Z   }

    static member (*) (v1 : Vector3, v2 : Vector3) =
        {   X = v1.X * v2.X ; Y = v1.X * v2.Y ; Z = v1.Z * v2.Z   }

    static member (*) (v : Vector3, a) =
        {   X = v.X * a ; Y = v.Y * a ; Z = v.Z * a   }

    static member (*) (a, v: Vector3) =
        {   X = v.X * a ; Y = v.Y * a ; Z = v.Z * a   }

    static member (/) (v1 : Vector3, v2 : Vector3) =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y ; Z = v1.Z / v2.Z   }

    static member (/) (v : Vector3, a) =
        let invDiv = 1.0 / a
        {   X = v.X * invDiv ; Y = v.Y * invDiv ; Z = v.Z * invDiv   }

type Vector4 =
    {   X : float
        Y : float
        Z : float
        W : float   }
    with
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W)

    member this.LengthSquared =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W

    static member (-) (v1 : Vector4, v2 : Vector4) =
        {   X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z ; W = v1.W - v2.W   }

    static member (+) (v1 : Vector4, v2 : Vector4) =
        {   X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z ; W = v1.W + v2.W   }

    static member (~-) (v : Vector4) =
        {   X = -v.X ; Y = -v.Y ; Z = -v.Z ; W = v.W   }

    static member (*) (v1 : Vector4, v2 : Vector4) =
        {   X = v1.X * v2.X ; Y = v1.X * v2.Y ; Z = v1.Z * v1.Z ; W = v1.W * v2.W   }

    static member (*) (v : Vector4, a) =
        {   X = v.X * a  ; Y = v.Y * a ; Z = v.Z * a ; W = v.W * a   }

    static member (*) (a, v: Vector4) =
        {   X = v.X * a  ; Y = v.Y * a ; Z = v.Z * a ; W = v.W * a   }

    static member (/) (v1 : Vector4, v2 : Vector4) =
        {   X = v1.X / v2.X ; Y = v1.Y / v2.Y ; Z = v1.Z / v2.Z ; W = v1.W / v2.W   }

    static member (/) (v : Vector4, a) =
        let invDiv = 1.0 / a
        {   X = v.X * invDiv ; Y = v.Y * invDiv ; Z = v.Z * invDiv ; W = v.W / invDiv   }

// TODO: Need to convert this to use mathematically correct column vectors in place of row vectors

type Matrix3x2 =
    {   M11 : float ; M12 : float
        M21 : float ; M22 : float
        M31 : float ; M32 : float   }
    with
    member this.IsIdentity = // Check diagonal element first for early out.
        this.M11 = 1.0 && this.M22 = 1.0 &&
        this.M12 = 0.0 && this.M21 = 0.0 &&
        this.M31 = 0.0 && this.M32 = 0.0

    member this.Translation =
        {   Vector2.X = this.M31 ; Y = this.M32   }

    member this.Determinant =
        (this.M11 * this.M22) - (this.M21 * this.M12)

    static member (~-) (v : Matrix3x2) =
        {   M11 = -v.M11 ; M12 = -v.M12
            M21 = -v.M21 ; M22 = -v.M22
            M31 = -v.M31 ; M32 = -v.M32   }

    static member (+) (v1 : Matrix3x2, v2 : Matrix3x2) =
        {   M11 = v1.M11 + v2.M11 ; M12 = v1.M12 + v2.M12
            M21 = v1.M21 + v2.M21 ; M22 = v1.M22 + v2.M22
            M31 = v1.M31 + v2.M31 ; M32 = v1.M32 + v2.M32   }

    static member (-) (v1 : Matrix3x2, v2 : Matrix3x2) =
        {   M11 = v1.M11 - v2.M11 ; M12 = v1.M12 - v2.M12
            M21 = v1.M21 - v2.M21 ; M22 = v1.M22 - v2.M22
            M31 = v1.M31 - v2.M31 ; M32 = v1.M32 - v2.M32   }

    static member (*) (v1 : Matrix3x2, v2 : Matrix3x2) =
        {   M11 = v1.M11 * v2.M11 + v1.M12 * v2.M21
            M12 = v1.M11 * v2.M12 + v1.M12 * v2.M22
            M21 = v1.M21 * v2.M11 + v1.M22 * v2.M21
            M22 = v1.M21 * v2.M12 + v1.M22 * v2.M22
            M31 = v1.M31 * v2.M11 + v1.M32 * v2.M21 + v2.M31
            M32 = v1.M31 * v2.M12 + v1.M32 * v2.M22 + v2.M32   }

    static member (*) (v1 : Matrix3x2, v2 : float) =
        {   M11 = v1.M11 * v2 ; M12 = v1.M12 * v2
            M21 = v1.M21 * v2 ; M22 = v1.M22 * v2
            M31 = v1.M31 * v2 ; M32 = v1.M32 * v2   }

type Matrix4x4 =
    {   M11 : float ; M12 : float ; M13 : float ; M14 : float
        M21 : float ; M22 : float ; M23 : float ; M24 : float
        M31 : float ; M32 : float ; M33 : float ; M34 : float
        M41 : float ; M42 : float ; M43 : float ; M44 : float   }
    with
    member this.IsIdentity = // Check diagonal element first for early out.
        this.M11 = 1.0 && this.M22 = 1.0 && this.M33 = 1.0 && this.M44 = 1.0 &&
        this.M12 = 0.0 && this.M13 = 0.0 && this.M14 = 0.0 && this.M21 = 0.0 &&
        this.M23 = 0.0 && this.M24 = 0.0 && this.M31 = 0.0 && this.M32 = 0.0 &&
        this.M34 = 0.0 && this.M41 = 0.0 && this.M42 = 0.0 && this.M43 = 0.0

    member this.Translation : Vector3 =
        {   X = this.M41 ; Y = this.M42 ; Z = this.M43   }

    member this.Determinant =
        let a, b, c, d,
            e, f, g, h,
            i, j, k, l,
            m, n, o, p = 
                this.M11, this.M12, this.M13, this.M14,
                this.M21, this.M22, this.M23, this.M24,
                this.M31, this.M32, this.M33, this.M34,
                this.M41, this.M42, this.M43, this.M44

        let kp_lo = k * p - l * o
        let jp_ln = j * p - l * n
        let jo_kn = j * o - k * n
        let ip_lm = i * p - l * m
        let io_km = i * o - k * m
        let in_jm = i * n - j * m

        let a11 = +(f * kp_lo - g * jp_ln + h * jo_kn)
        let a12 = -(e * kp_lo - g * ip_lm + h * io_km)
        let a13 = +(e * jp_ln - f * ip_lm + h * in_jm)
        let a14 = -(e * jo_kn - f * io_km + g * in_jm)

        a * a11 + b * a12 + c * a13 + d * a14

    static member (~-) (v : Matrix4x4) =
        {   M11 = -v.M11 ; M12 = -v.M12 ; M13 = -v.M13 ; M14 = -v.M14
            M21 = -v.M21 ; M22 = -v.M22 ; M23 = -v.M23 ; M24 = -v.M24
            M31 = -v.M31 ; M32 = -v.M32 ; M33 = -v.M33 ; M34 = -v.M34
            M41 = -v.M41 ; M42 = -v.M42 ; M43 = -v.M43 ; M44 = -v.M44   }

    static member (+) (v1 : Matrix4x4, v2 : Matrix4x4) =
        {   M11 = v1.M11 + v2.M11 ; M12 = v1.M12 + v2.M12 ; M13 = v1.M13 + v2.M13 ; M14 = v1.M14 + v2.M14
            M21 = v1.M21 + v2.M21 ; M22 = v1.M22 + v2.M22 ; M23 = v1.M23 + v2.M23 ; M24 = v1.M24 + v2.M24
            M31 = v1.M31 + v2.M31 ; M32 = v1.M32 + v2.M32 ; M33 = v1.M33 + v2.M33 ; M34 = v1.M34 + v2.M34
            M41 = v1.M41 + v2.M41 ; M42 = v1.M42 + v2.M42 ; M43 = v1.M43 + v2.M43 ; M44 = v1.M44 + v2.M44   }

    static member (-) (v1 : Matrix4x4, v2 : Matrix4x4) =
        {   M11 = v1.M11 - v2.M11 ; M12 = v1.M12 - v2.M12 ; M13 = v1.M13 - v2.M13 ; M14 = v1.M14 - v2.M14
            M21 = v1.M21 - v2.M21 ; M22 = v1.M22 - v2.M22 ; M23 = v1.M23 - v2.M23 ; M24 = v1.M24 - v2.M24
            M31 = v1.M31 - v2.M31 ; M32 = v1.M32 - v2.M32 ; M33 = v1.M33 - v2.M33 ; M34 = v1.M34 - v2.M34
            M41 = v1.M41 - v2.M41 ; M42 = v1.M42 - v2.M42 ; M43 = v1.M43 - v2.M43 ; M44 = v1.M44 - v2.M44   }

    static member (*) (v1 : Matrix4x4, v2 : Matrix4x4) =
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

    static member (*) (v1 : Matrix4x4, v2 : float) =
        {   M11 = v1.M11 * v2 ; M12 = v1.M12 * v2 ; M13 = v1.M13 * v2 ; M14 = v1.M14 * v2
            M21 = v1.M21 * v2 ; M22 = v1.M22 * v2 ; M23 = v1.M23 * v2 ; M24 = v1.M24 * v2
            M31 = v1.M31 * v2 ; M32 = v1.M32 * v2 ; M33 = v1.M33 * v2 ; M34 = v1.M34 * v2
            M41 = v1.M41 * v2 ; M42 = v1.M42 * v2 ; M43 = v1.M43 * v2 ; M44 = v1.M44 * v2   }

type Quaternion =
    {   X : float
        Y : float
        Z : float
        W : float   }
    with
    member this.IsIdentity =
        this.X = 0.0 && this.Y = 0.0 && this.Z = 0.0 && this.W = 1.0

    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W)

    member this.LengthSquared =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W

    static member (~-) (value : Quaternion) : Quaternion =
        {   X = -value.X ; Y = -value.Y ; Z = -value.Z ; W = -value.W   }

    static member (+) (value1 : Quaternion, value2 : Quaternion) =
        {   X = value1.X + value2.X ; Y = value1.Y + value2.Y
            Z = value1.Z + value2.Z ; W = value1.W + value2.W   }


    static member (-) (value1 : Quaternion, value2 : Quaternion) =
        {   X = value1.X - value2.X; Y = value1.Y - value2.Y
            Z = value1.Z - value2.Z; W = value1.W - value2.W   }


    static member (*) (value1 : Quaternion, value2 : Quaternion) =
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

    static member (*) (value1 : Quaternion, value2 : float) =
        {   X = value1.X * value2
            Y = value1.Y * value2
            Z = value1.Z * value2
            W = value1.W * value2   }

    static member (/) (value1 : Quaternion, value2 : Quaternion) =
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


type Plane =
    {   Normal : Vector3
        D : float   }
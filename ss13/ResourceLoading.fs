module ss13.ResourceLoading

open System
open System.Collections.Generic
open System.IO
open System.IO.MemoryMappedFiles
open System.Runtime.InteropServices

open SharpBgfx

let RootPath = "../../../Assets/"

let GetShaderPath () =
    match Bgfx.GetCurrentBackend () with
    | RendererBackend.Direct3D11
    | RendererBackend.Direct3D12 ->
        Path.Combine(RootPath, "Shaders/bin/dx11/")
    | RendererBackend.OpenGL ->
        Path.Combine(RootPath, "Shaders/bin/glsl/")
    | RendererBackend.OpenGLES ->
        Path.Combine(RootPath, "Shaders/bin/gles/")
    | RendererBackend.Direct3D9 ->
        Path.Combine(RootPath, "Shaders/bin/dx9/")
    | _ ->
        Path.Combine(RootPath, "Shaders/bin/glsl/")

let LoadShader name =
    let path = Path.Combine(GetShaderPath(), name) + ".bin"
    let mem = MemoryBlock.FromArray(File.ReadAllBytes(path))
    new Shader (mem)

let LoadProgram vsName fsName =
    let vsh = LoadShader vsName
    let fsh = LoadShader fsName

    new Program (vsh, fsh, true)

let LoadComputeProgram csName =
    let csh = LoadShader csName
    new Program (csh, true)

let LoadTexture name =
    let path = Path.Combine(RootPath, "textures/", name)
    let mem = MemoryBlock.FromArray(File.ReadAllBytes(path))
    Texture.FromFile (mem, TextureFlags.None, 0)

//let LoadMesh fileName =
//    let path = Path.Combine(RootPath, "meshes/", fileName)
//    let groups : List<MeshGroup> = list.Empty
//    let group = MeshGroup ()
//    VertexLayout layout = null
//
//    using (var file = MemoryMappedFile.CreateFromFile(path)) {
//        var reader = file.CreateViewAccessor();
//        var bytes = new FileInfo(path).Length;
//        var index = 0;
//
//        while (index < bytes) {
//            var tag = reader.ReadUInt32(index); index += sizeof(uint);
//            if (tag == ChunkTagVB) {
//                // skip bounding volume info
//                index += BoundingVolumeSize;
//
//                layout = reader.ReadVertexLayout(ref index);
//
//                var vertexCount = reader.ReadUInt16(ref index);
//                var vertexData = reader.ReadArray<byte>(vertexCount * layout.Stride, ref index);
//                group.VertexBuffer = new VertexBuffer(MemoryBlock.FromArray(vertexData), layout);
//            }
//            else if (tag == ChunkTagIB) {
//                var indexCount = reader.ReadUInt32(ref index);
//                var indexData = reader.ReadArray<ushort>((int)indexCount, ref index);
//
//                group.IndexBuffer = new IndexBuffer(MemoryBlock.FromArray(indexData));
//            }
//            else if (tag == ChunkTagPri) {
//                // skip material name
//                var len = reader.ReadUInt16(ref index);
//                index += len;
//
//                // read primitive data
//                var count = reader.ReadUInt16(ref index);
//                for (int i = 0; i < count; i++) {
//                    // skip name
//                    len = reader.ReadUInt16(ref index);
//                    index += len;
//
//                    var prim = reader.Read<Primitive>(ref index);
//                    group.Primitives.Add(prim);
//
//                    // skip bounding volumes
//                    index += BoundingVolumeSize;
//                }
//
//                groups.Add(group);
//                group = new MeshGroup();
//            }
//        }
//    }
//
//    return new Mesh(layout, groups);
//}

let MakeFourCC (a : char) (b : char) (c : char) (d : char) =
    uint32 a ||| uint32 b <<< 8 ||| uint32 c <<< 16 ||| uint32 d <<< 24

let BoundingVolumeSize = 26 * sizeof<float>
let ChunkTagVB = MakeFourCC 'V' 'B' ' ' '\u0001'
let ChunkTagIB = MakeFourCC 'I' 'B' ' ' '\u0000'
let ChunkTagPri = MakeFourCC 'P' 'R' 'I' '\u0000'


type ResourceContainer<'a, 'b when 'a : (new : unit -> 'a) and 'b : comparison> () =
    let mutable resourceMap : Map<'b, 'a> = Map.empty

    do ()

    member private this.insertResource id resource = ()

//    member this.Load (id, filePath) =
//        let resource = new 'a (filePath)
//        this.insertResource resource
//        
//
//    member this.Load<'c> (id, filePath, parameter) = ()
//
//    member this.Get id = ()
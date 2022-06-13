module Crochetgen.ImageFormatting

open Crochetgen.Pixel.Utils

let sharpenImage granularity image = 
    image
    |> Array.map (roundPixel granularity)

let unflattenImage width height =

    let to2DArray (chunks: array<array<'a>>) =
        Array2D.init height width (fun i j -> chunks[i][j])

    Array.chunkBySize width
    >> to2DArray

let unpackRows image=

    let height =
        Array2D.length1 image

    let rowSlice i =
        image[i,*]
    
    rowSlice
    |> Seq.init height

let flattenImage image =
    image
    |> unpackRows
    |> Seq.concat
    |> Array.ofSeq

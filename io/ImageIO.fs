module Crochetgen.ImageIO

open Crochetgen.Errors
open Crochetgen.ImageInterop
open Crochetgen.ImageFormatting

let imageIsNotEmpty image =
    match Array.isEmpty image with
    | false -> image |> Ok
    | true -> EmptyImage |> Error

let imageToPixels targetWidth targetHeight =
    resizeImage targetWidth targetHeight
    >> Result.map getPixelArray
    >> Result.bind imageIsNotEmpty

let loadImage = loadImage

let savePixels filename format width height =
    flattenImage
    >> savePixelsToImage filename format width height

let makeOutFilename name format =
    appendExtension name format

module Crochetgen.ImageIO

open Crochetgen.Errors
open Crochetgen.ImageInterop
open Crochetgen.ImageFormatting

let imageIsNotEmpty image =
    match Array.isEmpty image with
    | false -> image |> Ok
    | true -> EmptyImage |> Error

let loadPixelDataFromImageFile targetWidth targetHeight =
    loadImage
    >> Result.bind (resizeImage targetWidth targetHeight)
    >> Result.map getPixelArray
    >> Result.bind imageIsNotEmpty

let savePixels filename width height =
    flattenImage
    >> savePixelsToImage filename width height
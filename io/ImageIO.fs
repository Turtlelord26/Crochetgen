module Crochetgen.ImageIO

open Crochetgen.Errors
open Crochetgen.ImageInterop
open Crochetgen.ImageFormatting

let seqIsNotEmpty seq =
    match Seq.isEmpty seq with
    | false -> seq |> Ok
    | true -> EmptyImage |> Error

let loadPixelDataFromImageFile targetWidth targetHeight =
    loadImage
    >> Result.bind (resizeImage targetWidth targetHeight)
    >> Result.map getPixelArray
    >> Result.bind seqIsNotEmpty

let savePixels filename width height =
    decompressAndFlattenImageRows
    >> savePixelsToImage filename width height
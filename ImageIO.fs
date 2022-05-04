module Crochetgen.ImageIO

open Crochetgen.Errors
open Crochetgen.ImageInterop

let seqIsNotEmpty seq =
    match Seq.isEmpty seq with
    | false -> seq |> Ok
    | true -> EmptyImage |> Error

let loadPixelDataFromImageFile targetWidth targetHeight =
    loadImage
    >> Result.bind (resizeImage targetWidth targetHeight)
    >> Result.map getPixelArray
    >> Result.bind seqIsNotEmpty

let savePixels filename width height image =
    savePixelsToImage filename width height image
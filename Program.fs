module Crochetgen.Main

open Crochetgen.Errors.Fail
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.Stitches
open Crochetgen.ImageIO
open Crochetgen.ImageSimplification
open Crochetgen.Writer

let generatePattern numColors outPath width height =
    processImage numColors width height
    >> makeStitchesFromPixels width
    >> makePattern
    >> writeOutput outPath

let run inPath numColors targetWidth targetHeight outPath =
    match loadPixelDataFromImageFile targetWidth targetHeight inPath with
    | Ok image -> generatePattern numColors outPath targetWidth targetHeight image
    | Error e -> fail e

let unpackAndRun (argv: string[]) =
    let inPath = argv[0]
    let numColors = int argv[1]
    let targetWidth = int argv[2]
    let targetHeight = int argv[3]
    let outPath = argv[4]
    run inPath numColors targetWidth targetHeight outPath

let validateThenRun argv =
    match argv |> validateInput with
    | Some errors -> Some errors
    | None -> unpackAndRun argv

[<EntryPoint>]
let main argv =
    match validateThenRun argv with
    | Some errors -> errors |> writeErrors; 1
    | None -> printfn "Complete!"; 0

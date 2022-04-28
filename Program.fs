module Crochetgen.Main

open Crochetgen.Errors.Fail
open Crochetgen.InputValidation
open Crochetgen.Operators
open Crochetgen.Pattern
open Crochetgen.Stitches
open Crochetgen.ImageInterop
open Crochetgen.ImageSimplification
open Crochetgen.Writer

let generatePattern numColors outPath width =
    processImage numColors
    >> makeStitchesFromPixels width
    >> makePattern
    >> writeStitches outPath

let run inPath numColors targetWidth targetHeight outPath =
    match loadPixelDataFromImageFile inPath targetWidth targetHeight with
    | Ok image -> generatePattern numColors outPath targetWidth image
    | Error e -> fail e

let runIfValid (argv: string[]) =
    let inPath = argv[0]
    let numColors = int argv[1]
    let targetWidth = int argv[2]
    let targetHeight = int argv[3]
    let outPath = argv[4]
    run inPath numColors targetWidth targetHeight outPath

[<EntryPoint>]
let main argv =
    match chain (runIfValid argv) validateInput argv with
    | Some errors -> errors |> outputErrors; 1
    | None -> printfn "Complete!"; 0

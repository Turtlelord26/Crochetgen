module Crochetgen.Main

open Crochetgen.Errors.Fail
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.Stitches
open Crochetgen.ImageInterop
open Crochetgen.ImageSimplification
open Crochetgen.Writer

//https://docs.sixlabors.com/articles/imagesharp/pixelbuffers.html
//https://docs.sixlabors.com/api/ImageSharp/SixLabors.ImageSharp.PixelFormats.Rgb24.html

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

let chain option func arg =
    match option with
    | None -> func arg
    | Some errors -> Some errors

[<EntryPoint>]
let main argv =
    match chain (runIfValid argv) validateInput argv with
    | Some errors -> errors |> outputErrors; 1
    | None -> printfn "Complete!"; 0

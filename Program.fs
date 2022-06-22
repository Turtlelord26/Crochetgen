open Crochetgen.ColorSelection
open Crochetgen.ColorSimplification
open Crochetgen.ColorSmoothening
open Crochetgen.Errors.Fail
open Crochetgen.Errors.OptionUtils
open Crochetgen.ImageIO
open Crochetgen.ImageFormatting
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.RailwayUtilities
open Crochetgen.Stitches
open Crochetgen.Writer

let run numColors width height outName format image =

    let sharpenedImage = 
        image
        |> sharpenImage 8
    
    let colorSet = 
        sharpenedImage
        |> selectColors numColors
    
    let smoothenImage =
        smoothenColors 1 3
        >> smoothenColors 2 1
        >> smoothenColors 3 1
        >> smoothenColors 2 1
        >> smoothenColors 1 2
        >> smoothenColors 2 1
        >> smoothenColors 1 2

    let patternPipeline sharpenedImage  =
        sharpenedImage
        |> unflattenImage width height
        |> simplifyColors colorSet
        |> smoothenImage
        |> tee (savePixels (makeOutFilename outName format) format width height)
        |> makeStitchesFromPixels
        |> makePattern
        |> writeOutput (outName + ".txt")
    
    patternPipeline sharpenedImage
    ++ (writeOutput (outName + "_colors.txt") (colorSet |> printFormatColorSelection))

let loadImageThenRun' image format numColors width height outName =
    match imageToPixels width height image with
    | Ok pixels -> pixels |> run numColors width height outName format
    | Error e -> fail e

let loadImageThenRun inPath numColors width height outName =
    match loadImage inPath with
    | Ok (image, format) -> loadImageThenRun' image format numColors width height outName
    | Error e -> fail e

let unpackAndRun (argv: string[]) =
    let inPath = argv[0]
    let numColors = int argv[1]
    let width = int argv[2]
    let height = int argv[3]
    let outName = argv[4]
    loadImageThenRun inPath numColors width height outName

let validateThenRun argv =
    match argv |> validateInput with
    | Some errors -> Some errors
    | None -> unpackAndRun argv

[<EntryPoint>]
let main argv =
    match validateThenRun argv with
    | Some errors -> errors |> writeErrors; 1
    | None -> printfn "Complete!"; 0

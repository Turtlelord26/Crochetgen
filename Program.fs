open Crochetgen.ColorSelection
open Crochetgen.ColorSimplification
open Crochetgen.ColorSmoothening
open Crochetgen.Errors.Fail
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

    let patternPipeline =
        unflattenImage width height
        >> simplifyColors colorSet
        >> smoothenImage
        >> tee (savePixels (makeOutFilename outName format) format width height)
        >> makeStitchesFromPixels
        >> makePattern
        >> writeOutput (outName + ".txt")
    
    let saveColorSet colorSet image = 
        writeOutput (outName + "_colors.txt") (colorSet |> printFormatColorSelection)
    
    patternPipeline sharpenedImage
    |> saveColorSet colorSet

let unpackAndRun (argv: string[]) =
    let inPath = argv[0]
    let numColors = int argv[1]
    let width = int argv[2]
    let height = int argv[3]
    let outName = argv[4]

    match loadImage inPath with
    | Ok (image, format) ->
        image
        |> imageToPixels width height
        |> Result.bind (run numColors width height outName format)
    | Error e -> Error e

let validateThenRun argv =
    match argv |> validateInput with
    | Some errors -> Some errors
    | None -> 
        match unpackAndRun argv with
        | Ok () -> None
        | Error e -> fail e

[<EntryPoint>]
let main argv =
    match validateThenRun argv with
    | Some errors -> errors |> writeErrors; 1
    | None -> printfn "Complete!"; 0

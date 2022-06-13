open Crochetgen.ColorSelection
open Crochetgen.ColorSimplification
open Crochetgen.ColorSmoothening
open Crochetgen.Errors.Fail
open Crochetgen.Errors.OptionUtils
open Crochetgen.ImageIO
open Crochetgen.ImageFormatting
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.Stitches
open Crochetgen.Writer

let mapDeadEnd func arg =
    func arg
    arg

let run numColors width height outName image =

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
        |> mapDeadEnd (savePixels (outName + ".png") width height)
        |> makeStitchesFromPixels
        |> makePattern
        |> writeOutput (outName + ".txt")
    
    patternPipeline sharpenedImage
    ++ (writeOutput (outName + "_colors.txt") (colorSet |> printFormatColorSelection))

let loadImageThenRun inPath numColors width height outName =
    match loadPixelDataFromImageFile width height inPath with
    | Ok image -> image |> run numColors width height outName
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

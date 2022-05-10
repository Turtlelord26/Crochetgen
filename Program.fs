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
        |> sharpenImage
    
    let colorSet = 
        sharpenedImage
        |> selectColors numColors

    let patternPipeline =
        unflattenAndCompressImageRows width
        >> simplifyColors colorSet
        >> smoothenColors
        >> mapDeadEnd (savePixels (outName + ".png") width height)
        >> makeStitchesFromPixels
        >> makePattern
        >> writeOutput (outName + ".txt")
    
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

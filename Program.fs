open Crochetgen.ColorSelection
open Crochetgen.ColorSimplification
open Crochetgen.ColorSmoothening
open Crochetgen.Errors.Fail
open Crochetgen.Errors.OptionUtils
open Crochetgen.ImageIO
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Flatten
open Crochetgen.PixelCount.Utils
open Crochetgen.Stitches
open Crochetgen.Writer

let mapDeadEnd func arg =
    func arg
    arg

let run numColors width height outName image =

    let sharpenedPixels = 
        image
        |> Seq.map (roundPixel 8)
        |> Seq.cache
    
    let makeColorSet =
        Seq.countBy id
        >> Seq.map ((<||) makePixelCount)
        >> selectColors numColors
    
    let colorSet =
        sharpenedPixels
        |> makeColorSet 

    let patternPipeline =
        unflattenAndCompressImageRows width
        >> simplifyColors colorSet
        >> smoothenColors
        >> mapDeadEnd (savePixels (outName + ".png") width height)
        >> makeStitchesFromPixels
        >> makePattern
        >> writeOutput (outName + ".txt")
    
    patternPipeline sharpenedPixels

let loadImageAndRun inPath numColors width height outName =
    match loadPixelDataFromImageFile width height inPath with
    | Ok image -> run numColors width height outName image
    | Error e -> fail e

let unpackAndRun (argv: string[]) =
    let inPath = argv[0]
    let numColors = int argv[1]
    let width = int argv[2]
    let height = int argv[3]
    let outName = argv[4]
    loadImageAndRun inPath numColors width height outName

let validateThenRun argv =
    match argv |> validateInput with
    | Some errors -> Some errors
    | None -> unpackAndRun argv

[<EntryPoint>]
let main argv =
    match validateThenRun argv with
    | Some errors -> errors |> writeErrors; 1
    | None -> printfn "Complete!"; 0

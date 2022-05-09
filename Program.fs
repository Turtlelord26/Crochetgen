open Crochetgen.ColorSelection
open Crochetgen.ColorSimplification
open Crochetgen.ColorSmoothening
open Crochetgen.Errors.Fail
open Crochetgen.Errors.OptionUtils
open Crochetgen.ImageIO
open Crochetgen.InputValidation
open Crochetgen.Pattern
open Crochetgen.PixelCount.Flatten
open Crochetgen.Stitches
open Crochetgen.Writer

let mapDeadEnd func arg =
    func arg
    arg

let run numColors width height outName =
    simplifyColors numColors width
    >> smoothenColors
    >> mapDeadEnd (savePixels (outName + ".png") width height)
    >> makeStitchesFromPixels
    >> makePattern
    >> writeOutput (outName + ".txt")

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

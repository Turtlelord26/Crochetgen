module Crochetgen.Writer

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Errors.Fail
open Crochetgen.Errors.Print

let outputErrors errors =
    errors
    |> printErrors
    |> printfn "%s"

let writer filepath =
    try
        File.CreateText(filepath)
        |> Ok
    with
    | :? UnauthorizedAccessException -> filepath |> UnauthorizedAccess |> Error
    | :? ArgumentNullException -> PathNull |> Error
    | :? PathTooLongException -> PathTooLong |> Error
    | :? FileNotFoundException as e -> e.Message |> FileNotFound |> Error
    | :? DirectoryNotFoundException -> filepath |> DirectoryNotFound |> Error
    | :? NotSupportedException -> filepath |> MalformedPath |> Error

let write (patternText: string) (writer: StreamWriter) =
    try
        writer.Write(patternText)
        writer.Close()
        None
    with
    | :? ObjectDisposedException as e -> e.Message |> ObjectDisposed |> fail
    | :? NotSupportedException -> WriteNotSupported |> fail
    | :? IOException -> patternText |> WriteIO |> fail

let writeStitches filepath patternText =
    match writer filepath with
    | Ok stream -> write patternText stream
    | Error e -> e |> fail
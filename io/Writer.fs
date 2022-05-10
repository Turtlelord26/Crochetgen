module Crochetgen.Writer

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Errors.Fail
open Crochetgen.Errors.Print

let writeErrors errors =
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
    | :? DirectoryNotFoundException as e -> e.Message |> DirectoryNotFound |> Error
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

let writeOutput filepath outText =
    match writer filepath with
    | Ok stream -> write outText stream
    | Error e -> e |> fail
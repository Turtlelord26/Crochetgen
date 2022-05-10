namespace Crochetgen.Errors

type Error =
    | DirectoryNotFound of message: string
    | EmptyImage
    | ImageContentInvalid of filename: string
    | ImageFilenameNull
    | ImageFormatNotRecognized of filename: string
    | ImageFormatNotSupported of filename: string
    | ImageProcessingFailure of message: string
    | IntegerInputNull
    | IntegerOverflow of input: string
    | IntegerParsingError of input: string
    | InteropNullPointer of message: string
    | MalformedInput
    | MalformedPath of filepath: string
    | NonpositiveIntegerInput of input: string
    | PathNull
    | PathTooLong
    | PixelDimensionsMismatch of message: string
    | ObjectDisposed of message: string
    | FileNotFound of message: string
    | UnauthorizedAccess of filepath: string
    | WriteIO of outText: string
    | WriteNotSupported

type ErrorList = { errors: Error list }

module Fail =

    let fail error = 
        { errors=[error] } |> Some
    
module OptionUtils =

    let addOptions option1 option2 =

        let addErrors error1 error2 =
            { errors = List.append error1.errors error2.errors }

        match option1, option2 with
        | None, None -> None
        | Some e, None -> Some e
        | None, Some f -> Some f
        | Some e, Some f -> addErrors e f |> Some
    
    let (++) = addOptions

module Print =

    open Crochetgen.StringUtils

    let printError error =
        match error with
        | DirectoryNotFound message -> message
        | EmptyImage -> "No information loaded from image file, presumed empty."
        | FileNotFound message -> message
        | ImageContentInvalid filename -> $"Image content of file {filename} invalid."
        | ImageFilenameNull -> "Image filename null."
        | ImageFormatNotRecognized filename -> $"Image format of file {filename} not recognized."
        | ImageFormatNotSupported filename -> $"Image format of file {filename} not supported."
        | ImageProcessingFailure message -> message
        | IntegerInputNull -> $"An integer input was null."
        | IntegerOverflow number -> $"{number} overflows."
        | IntegerParsingError number -> $"{number} is not a parseable integer."
        | InteropNullPointer message -> message
        | MalformedInput -> "Usage: Crochetgen imageFilePath numberOfColors widthInStitches heightInStitches outputFileName"
        | MalformedPath filepath -> $"{filepath} is in an invalid format."
        | NonpositiveIntegerInput input -> $"Input {input} is nonpositive. Integer inputs must be positive."
        | ObjectDisposed message -> message
        | PathNull -> "Path was null."
        | PathTooLong -> "The specified path, file name, or both exceed the system-defined maximum length."
        | PixelDimensionsMismatch message -> "Length of pixel array did not match given width * height while saving an image\n" + message
        | UnauthorizedAccess filepath -> $"User is not authorized to write to {filepath}, or it is readonly or hidden."
        | WriteIO outText -> $"An I/O error occurred while writing the output below.\n{outText}"
        | WriteNotSupported -> "AutoFlush is true or the StreamWriter buffer is full, and the contents of the buffer cannot be written to the underlying fixed size stream because the StreamWriter is at the end the stream."

    let printErrors =

        let unpackErrors errorList = errorList.errors
        
        unpackErrors
        >> List.map printError
        >> List.reduce concatAsNewline
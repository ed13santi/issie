(*
    FilesIO.fs

    Utility functions to interact with files.
*)

module FilesIO

open Helpers
open CommonTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node

[<AutoOpen>]
module JsonHelpers =
    open Fable.SimpleJson

    let stateToJsonString (state : CanvasState) : string =
        SimpleJson.stringify state

    let jsonStringToState (jsonString : string) =
         Json.tryParseAs<CanvasState> jsonString

let private tryLoadStateFromPath (filePath: string) =
    fs.readFileSync(filePath, "utf8")
    |> jsonStringToState

/// Extract the labels and bus widths of the inputs and outputs nodes.
let private parseDiagramSignature canvasState
        : (string * int) list * (string * int) list =
    let rec extractIO
            (components : Component list)
            (inputs : (string * int) list)
            (outputs : (string * int) list) =
        match components with
        | [] -> inputs, outputs
        | comp :: components' ->
            match comp.Type with
            | Input width  -> extractIO components' ((comp.Label, width) :: inputs) outputs
            | Output width -> extractIO components' inputs ((comp.Label, width) :: outputs)
            | _ -> extractIO components' inputs outputs
    let components, _ = canvasState
    let inputs, outputs = extractIO components [] []
    List.rev inputs, List.rev outputs

let private getBaseNameNoExtension filePath =
    let baseName = path.basename filePath
    match baseName.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [baseName] -> baseName // No dots found.
    | firstSplit :: splits ->
        // Quite ugly but works.
        let rest =
            ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
                baseName + "." + splits.[i]
            )
        firstSplit + rest

let private projectFileFilters =
    createObj !![
        "name" ==> "DECAD project file"
        "extensions" ==> ResizeArray [ "dprj" ]
    ] 
    |> unbox<FileFilter> 
    |> Array.singleton

let private projectFilters =
    createObj !![ "name" ==> "DECAD project" ]
    |> unbox<FileFilter>
    |> Array.singleton

/// Ask the user to choose a project file, with a dialog window.
/// Return the folder containing the chosen project file.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath () : string option =
    let options = createEmpty<OpenDialogOptions>
    options.filters <- projectFileFilters

    electron.remote.dialog.showOpenDialogSync(options)
    |> Option.bind (
        Seq.toList
        >> function
        | [] -> Option.None
        | p :: _ -> Some <| path.dirname p
    )

/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let askForNewProjectPath () : string option =
    let options = createEmpty<SaveDialogOptions>
    options.filters <- projectFilters
    
    electron.remote.dialog.showSaveDialogSync(options)
    
let tryCreateFolder (path : string) =
    try
        Result.Ok <| fs.mkdirSync path
    with
        | ex -> Result.Error <| sprintf "%A" ex

let pathJoin args = path.join args
let basename filePath = path.basename filePath

/// Asyncronously remove file.
let removeFile folderPath baseName =
    let path = path.join [| folderPath; baseName + ".dgm" |]
    fs.unlink (U2.Case1 path, ignore) // Asynchronous.

/// Write utf8 encoded data to file.
/// Create file if it does not exist.
let writeFile path data =
    let options = createObj ["encoding" ==> "utf8"] |> Some
    fs.writeFileSync(path, data, options)

/// Save state to file. Automatically add the .dgm suffix.
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    writeFile path data

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName ([],[])

let private tryLoadComponentFromPath filePath =
    match tryLoadStateFromPath filePath with
    | Result.Error err -> Result.Error err
    | Result.Ok state ->
        let inputs, outputs = parseDiagramSignature state
        Result.Ok {
            Name = getBaseNameNoExtension filePath
            FilePath = filePath
            CanvasState = state
            InputLabels = inputs
            OutputLabels = outputs
        }

/// Try to load all diagram components from a file path.
/// Return a string with error if not possible.
let tryLoadComponentsFromPath folderPath : Result<LoadedComponent list, string> =
    fs.readdirSync (U2.Case1 folderPath)
    |> Seq.toList
    |> List.filter (path.extname >> ((=) ".dgm"))
    |> List.map (fun fileName ->
            path.join [| folderPath; fileName |] |> tryLoadComponentFromPath
        )
    |> tryFindError

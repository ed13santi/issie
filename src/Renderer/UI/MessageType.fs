module DiagramMessageType

open CommonTypes
open JSTypes
open SimulatorTypes
open Fable.React

type RightTab =
    | Properties
    | Catalogue
    | Simulation
    | WaveSim

type MemoryEditorData = {
    OnlyDiff : bool // Only show diffs in Memory Diff Viewer.
    Address : int option // Only show the specified memory address.
    NumberBase : NumberBase
}

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    Int2: int option
    MemorySetup : (int * int) option // AddressWidth, WordWidth. 
    MemoryEditorData : MemoryEditorData option // For memory editor and viewer.
}

type TopMenu = | Closed | Project | Files

//==========//
// Messages //
//==========//

// Messages that will be sent from JS code.
type JSDiagramMsg =
    | InitCanvas of JSCanvas // Has to be dispatched only once.
    | SelectComponent of JSComponent
    | UnselectComponent of unit
    | InferWidths of unit
    | SetHasUnsavedChanges of bool

// Messages that will be triggered on key combinations.
type KeyboardShortcutMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ

// WaveSim types 

type WaveName = string

type Wire = {
    NBits: uint32
    BitData: bigint 
}

type StateSample = string array
type Sample = | Wire of Wire | StateSample of StateSample
type SimTime = Sample array
type Waveform = Sample array


type WaveAdderModel = {
    /// generate data using this, which comes from makesimdata
    SimData : SimulationData option
    /// all the nets that exist in the currently simulated design
    Ports : NetGroup array;
    /// names for all ports in current design
    WaveNames : WaveName array
}

type WaveSimModel = {
    /// array of variable length (but always >= lastclock)
    SimData: SimulatorTypes.SimulationData array
    /// waveform names displayed, use findName
    WaveNames: string array
    /// react SVG for each waveform
    WaveTable: ReactElement array
    /// NetGroups displayed, as in WaveNames
    Ports: NetGroup array
    /// width of one clock in SVG units
    ClkWidth: float
    /// position of cursor (0 = first cycle)
    Cursor: uint32 
    /// tracks when the cursor text box is empty string
    CursorEmpty: bool
    /// for waveforms display
    Radix: NumberBase
    /// last clock cycle (index) of the generated SVG
    LastClk: uint32
    /// if Adder window is currently open (changing tab does not effect it)
    WaveAdderOpen: bool
    /// data needed by the waveform Edit window (adder)
    WaveAdder: WaveAdderModel option
    /// the circuit that is being simulated - the canvas may have changed
    LastCanvasState: CanvasState option 
}

let initWA = 
    { SimData = None; Ports = [||]; WaveNames = [||] }

let initWS: WaveSimModel =
    { SimData = [||]
      WaveNames = [||]
      WaveTable = [||]
      Ports = [||] 
      ClkWidth = 1.0
      Cursor = 0u
      CursorEmpty = false
      Radix = Bin
      LastClk = 9u 
      WaveAdderOpen = true
      WaveAdder = None
      LastCanvasState = None }

type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuNewFile
    | MenuZoom of float


/// Type for an open project which represents a complete design.
/// ProjectPath is directory containing project files.
/// OpenFileName is name of file from which current schematic sheet is loaded/saved, without extension or path
/// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
type Project = {
    /// directory which contains the project files
    ProjectPath : string
    /// name of open sheet (without extension)
    OpenFileName : string
    /// componnets have one-one correspondence with files
    LoadedComponents : LoadedComponent list
}



type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | SetCurrFileWSMod of WaveSimModel
    | SetWSError of SimulationError option
    | AddWaveSimFile of string * WaveSimModel
    | SetSimulationGraph of SimulationGraph
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick
    | EndSimulation
    | EndWaveSim
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | CloseProject
    | ShowPopup of (PopupDialogData -> ReactElement)
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogInt of int option
    | SetPopupDialogTwoInts of (int option * IntMode)
    | SetPopupDialogMemorySetup of (int * int) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | CloseDiagramNotification
    | SetSimulationNotification of ((Msg -> unit) -> ReactElement)
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
    | SetMemoryEditorNotification of ((Msg -> unit) -> ReactElement)
    | CloseMemoryEditorNotification
    | SetPropertiesNotification of ((Msg -> unit) -> ReactElement)
    | ClosePropertiesNotification
    | SetTopMenu of TopMenu
    | ReloadSelectedComponent of int
    | SetDragMode of DragMode
    | SetViewerWidth of int
    | MenuAction of MenuCommand * (Msg -> unit)
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetSimIsStale of bool
    | SetSimInProgress of Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}>
    | SetWaveSimModel of Sheet: string * WSModel: WaveSimModel
    | SimulateWhenInProgress of Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}>
    | SetSimNotInProgress
    | SetLastSimulatedCanvasState of CanvasState option
    | UpdateScrollPos of bool

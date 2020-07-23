(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module DiagramModelType

open CommonTypes
open DiagramMessageType
open SimulatorTypes
open Draw2dWrapper

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}

type Model = {
    Diagram : Draw2dWrapper
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    WaveSim : WaveSimModel option
    RightTab : RightTab
    Hilighted : ComponentId list * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    CreateComponentOffset : int // Change this offset every time a component is created to avoid overlaps.
    HasUnsavedChanges : bool
    CurrProject : Project option
    Popup : (PopupDialogData -> Fable.React.ReactElement) option
    PopupDialogData : PopupDialogData
    Notifications : Notifications
    TopMenu : TopMenu
}

module RendererTop

open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Core
open Fable.Core.JsInterop
open Electron
open JSHelpers
open Node






// Keybindings.
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu:
// https://www.electronjs.org/docs/tutorial/keyboard-shortcuts#keyboard-shortcuts 
// In our case the menu is invisible.
// I also tried setting up Electron global shortcuts, but that way the
// app was also capturing combinations of key pressed in other Electron windows
// open at the same time, such as VSCode (on Linux).

let makeItem (accelerator : string) (action : KeyboardEvent -> unit) =
    let handlerCaster f =
        System.Action<MenuItem, BrowserWindow, KeyboardEvent> f
    let item = createEmpty<MenuItemOptions>
    item.accelerator <- accelerator
    // These menu items will be invisible and only accessible via shortcuts.
    // Thanks to VisUAL2 code for lending this function to DEflow.
    item.click <- handlerCaster (fun _ _ -> action )
    

let invisibleMenu dispatch =
    let invisibleMenu = createEmpty<MenuItemOptions>
    let dispatch = DiagramMessageType.KeyboardShortcutMsg >> dispatch
    invisibleMenu.submenu <-
        !! [    
                makeItem "CmdOrCtrl+S" (fun _ -> dispatch DiagramMessageType.CtrlS)
                makeItem "Alt+C" (fun _ -> dispatch DiagramMessageType.AltC)
                makeItem "Alt+V" (fun _ -> dispatch DiagramMessageType.AltV)
                makeItem "Alt+Z" (fun _ -> dispatch DiagramMessageType.AltZ)
                makeItem "Alt+Shift+Z" (fun _ -> dispatch DiagramMessageType.AltShiftZ)
           ] |> U2.Case2 
    invisibleMenu



/// Create an invisible menu and attach keybindings to actions. 
/// Design decision: use Alt for actions that trigger equivalent to the buttons
/// on the diagram.
/// - copy diagram components: Alt+C
/// - paste diagram components: Alt+V
/// - undo diagram action: Alt+Z
/// - redo diagram action: Alt+Shift+Z
/// - save open file (not diagram action): Ctrl+S
let attachKeyShortcuts _ =
    let remote = electron.remote 
    let sub dispatch =
        let template: MenuItemOptions = invisibleMenu dispatch
        let menu = electron.Menu.buildFromTemplate [| U2.Case1 template |]
        remote.app.applicationMenu <- Some menu
    Cmd.ofSub sub





// This setup is useful to add other pages, in case they are needed.

type Model = DiagramModelType.Model

type Messages = DiagramMessageType.Msg



// -- Init Model

let init() = DiagramMainView.init()

// -- Create View

let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model = DiagramMainView.update msg model

let hello() =
    printfn "**** Starting Renderer ****"

hello()


Program.mkSimple init update view
|> Program.withReactBatched "electron-app"
|> Program.withSubscription attachKeyShortcuts
|> Program.run

﻿module Renderer

open Browser.Types
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Electron.Helpers



(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)
let mutable debugLevel = 0

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let argV =
        electron.remote.``process``.argv
        |> Seq.toList
        |> List.tail
        |> List.map (fun s -> s.ToLower())
    let isArg s = List.contains s argV
    debugLevel <-
        if isArg "--debug" || isArg "-d" then 2
        elif isArg "-w" then 1
        else 0

let menuSeparator =
   let sep = createEmpty<MenuItemOptions>
   sep.``type`` <- MenuItemType.Separator
   sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let handlerCaster f = System.Action<MenuItem, BrowserWindow, KeyboardEvent> f 
   let item = createEmpty<MenuItemOptions>
   item.label <- label
   match accelerator with | Some a -> item.accelerator <- a | _ -> ()
   item.click <- handlerCaster (fun _ _ keyEvent-> iAction keyEvent)
   item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item.visible <- cond
   item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- cond
   item


let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- label
        item.accelerator <- accelerator
        item.click <- fun _ _ _ -> action()


/// Make a new menu from a a list of menu items
let makeMenu (name : string) (table : MenuItemOptions list) =
   let subMenu = createEmpty<MenuItemOptions>
   subMenu.``type`` <- MenuItemType.SubMenu
   subMenu.label <- name
   subMenu.submenu <- U2.Case1 (table |> Array.ofList)
   subMenu


let viewMenu dispatch =
    setDebugLevel()
    let devToolsKey = if Node.Api.``process``.platform = Node.Base.Darwin then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.ToggleFullScreen
        menuSeparator
        makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeCondItem (debugLevel > 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.toggleDevTools())
    ]


// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch =
    let dispatch = DiagramMessageType.KeyboardShortcutMsg >> dispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- false
        invisibleMenu.submenu <-
<<<<<<< HEAD
            [| makeItem "CmdOrCtrl+S" (fun () -> dispatch DiagramMessageType.CtrlS)
               makeItem "Alt+C" (fun () -> dispatch DiagramMessageType.AltC)
               makeItem "Alt+V" (fun () -> dispatch DiagramMessageType.AltV)
               makeItem "Alt+Z" (fun () -> dispatch DiagramMessageType.AltZ)
               makeItem "Alt+Shift+Z" (fun () -> dispatch DiagramMessageType.AltShiftZ) |]
=======
            [| makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> dispatch DiagramMessageType.CtrlS)
               makeElmItem "Copy" "Alt+C" (fun () -> dispatch DiagramMessageType.AltC)
               makeElmItem "Paste" "Alt+V" (fun () -> dispatch DiagramMessageType.AltV)
               makeElmItem "Undo" "Alt+Z" (fun () -> dispatch DiagramMessageType.AltZ)
               makeElmItem "Redo" "Alt+Shift+Z" (fun () -> dispatch DiagramMessageType.AltShiftZ) |]
>>>>>>> master
            |> U2.Case1

let attachMenusAndKeyShortcuts dispatch =
    let sub dispatch =
        let menu =
            [|
                editMenu dispatch
                viewMenu()
            |]
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some false
        electron.remote.app.applicationMenu <- Some menu

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

Program.mkSimple init update view
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.run

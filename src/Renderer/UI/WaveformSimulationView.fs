(*
    WaveformSimulationView.fs

    View for waveform simulator in tab
*)

module WaveformSimulationView

open Fulma
open Fable.React
open Fable.React.Props

open DiagramMessageType
open DiagramStyle
open SimulatorTypes //Simulator is below as a module, is that a problem?

//type functions

let signalLength (signal: Signal) = 
    match snd signal with
    | OneBitSig lst -> List.length lst
    | ManyBitSig lst -> List.length lst

//initial model

//use this initially for debugging
let stdWave : Wave = 
    {
        wIn = "Signal-example", OneBitSig [One]
        cursorPos = uint 0
    }

let initModel : WaveSimModel = //modify the position constants
    {
        waves = [
                { stdWave with wIn = "Signal1", OneBitSig [One;Zero;Zero;One]}
                { stdWave with wIn = "Signal2", ManyBitSig [uint 4;uint 8;uint 3;uint 5;uint 0]}
            ]
        viewParams = {
            vPos      = uint 0
            vSize     = uint 0.5
            hPos      = uint 0
            hSize     = uint 1
            hNameSize = uint 2
            hValSize  = uint 8
            sigThick  = 0.02
            hBoxSize = uint 10
            vBoxSize  = uint 15
        }
        sigLimits = uint 0, uint 10
    }

// SVG functions

let makeLine (arg: LineParams) =
    line 
        [
        X1 (fst arg.pointA)
        Y1 (snd arg.pointA)
        X2 (fst arg.pointB)
        Y2 (snd arg.pointB)
        SVGAttr.Stroke arg.colour
        SVGAttr.StrokeWidth (string arg.thickness)
        ]
        []

let makeBox (x1,y1) (x2,y2) =
    rect
        [
        X x1
        Y y1
        SVGAttr.Width (x2-x1)
        SVGAttr.Height (y2-y1)
        SVGAttr.Stroke "black"
        SVGAttr.Fill "white"
        SVGAttr.StrokeWidth 0.1
        ]
        []

let makeSigLineArg pointA pointB =
    { dfltSigLine with pointA = pointA; pointB = pointB }

let makeSigLine pointA pointB =
    makeSigLineArg pointA pointB
    |> makeLine


//auxiliary functions to the viewer function

let needTransitionIntermediateLst (lstSignalValues: SigVals) =
    match lstSignalValues with
    | OneBitSig lst -> 
        List.zip lst.[0..lst.Length-2] lst.[1..lst.Length-1]
        |> List.map (fun (a,b) -> a <> b)
    | ManyBitSig lst -> 
        List.zip lst.[0..lst.Length-2] lst.[1..lst.Length-1]
        |> List.map (fun (a,b) -> a <> b)

let needTransitionAfter lstSignalValues =
    List.append (needTransitionIntermediateLst lstSignalValues) [false]

let needTransitionBefore lstSignalValues =
    List.append [true] (needTransitionIntermediateLst lstSignalValues)

let displayWaveform parameters (ind: int) wave  =
    let waveform = wave.wIn

    let nameLeft = float parameters.hPos
    let sigLeft = nameLeft + float parameters.hNameSize
    let spaceBetweenWaves = 0.5
    let sigHeight = 0.5//float parameters.vSize
    let sigTop = float parameters.vPos + float ind * (spaceBetweenWaves + sigHeight) + spaceBetweenWaves
    let sigBot = sigHeight + sigTop
    let sigCentre = (sigTop+sigBot) / 2.0
    let clkLen = float parameters.hSize
    let sigThick = parameters.sigThick
    let clkThick = 0.025
    let transLen = 0.1
    

    let makeBitSegment (hasTransition, (value: Bit, position: int)) = //choose better names
        let fPos = float position
        let fVal = 
            match value with
            | One  -> sigHeight 
            | Zero -> 0.0
        let left = fPos * clkLen + sigLeft
        let right = left + clkLen

        let signalLine = makeSigLine (left, sigBot-fVal) (right, sigBot-fVal) 

        match hasTransition with
        | true ->
            [makeSigLine (right, sigBot+sigThick/2.0) (right, sigTop-sigThick/2.0)]
            |> List.append [signalLine]
        | false ->  
            [signalLine]


    let makeBusSegment ((transitionBefore: bool, transitionAfter: bool), (value: uint, position: int)) = //choose better names
        let fPos = float position
        
        let leftLim = sigLeft + fPos * clkLen
        let rightLim = leftLim + clkLen
        let hCentre = (leftLim+rightLim) / 2.0
        let left =
            if transitionBefore then leftLim + transLen else leftLim
        let right = 
            if transitionAfter then rightLim - transLen else rightLim

        
        let top = makeSigLine (left, sigTop) (right, sigTop)
        let bottom = makeSigLine (left, sigBot) (right, sigBot)
        let topLeft = makeSigLine (leftLim, sigCentre) (left, sigTop)
        let bottomLeft = makeSigLine (leftLim, sigCentre) (left, sigBot)
        let topRight = makeSigLine (rightLim, sigCentre) (right, sigTop)
        let bottomRight = makeSigLine (rightLim, sigCentre) (right, sigBot)

        let clockMarkerLine = 
            let lineTmp = makeSigLineArg (rightLim, sigBot) (rightLim, sigTop)
            { lineTmp with colour = "gray"; thickness = clkThick }
            |> makeLine

        let text =
            text 
                [
                X hCentre
                Y (sigBot - sigHeight*0.1)
                SVGAttr.Fill "black"
                SVGAttr.FontSize (sigHeight * 0.8)
                SVGAttr.TextAnchor "middle"
                ]
                [ str <| string value ]

        match transitionBefore, transitionAfter with
        | true, true -> 
            [topLeft; bottomLeft; topRight; bottomRight]
        | true, false -> 
            [topLeft; bottomLeft]
        | false, true -> 
            [topRight; bottomRight]
        | false, false -> 
            []
        |> List.append [text; clockMarkerLine; top; bottom]
        //should there be the impossible case???

    let label =
        text 
            [
            X nameLeft
            Y sigCentre
            SVGAttr.Fill "black"
            SVGAttr.FontSize sigHeight
            SVGAttr.TextAnchor "start"
            ]
            [ str <| fst waveform ]
    
    let waveValues = snd waveform
    match waveValues with
    | OneBitSig lst ->
        List.zip lst [0..lst.Length-1]
        |> List.zip (needTransitionAfter waveValues)
        |> List.collect makeBitSegment 
    | ManyBitSig lst ->
        let transitionsTupleLst =
            List.zip (needTransitionBefore waveValues) (needTransitionAfter waveValues)
        List.zip lst [0..lst.Length-1]
        |> List.zip transitionsTupleLst
        |> List.collect makeBusSegment
    |> List.append [label]

let makeBackground parameters = 
    let width = float parameters.hBoxSize
    let height = float parameters.vBoxSize
    let top = float parameters.vPos
    let bot = top + height
    let left = float parameters.hPos + float parameters.hNameSize
    let right = left + width
    let clkThickness = 0.025 //TODO: change to variable
    let clkLen = float parameters.hSize
    
    let clkLine x = 
        { dfltSigLine with 
            pointA = x, top
            pointB = x, bot
            colour = "gray"
            thickness = clkThickness
        }
        |> makeLine

    [(int (left / clkLen) + 1)..1..(int (right / clkLen) + 1)]
    |> List.map ((fun x -> float x * clkLen) >> clkLine)
    |> List.append [makeBox (left,top) (right,bot)]

    
//view function of the waveform simulator

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    let startWaveSim () = 
        dispatch <| StartWaveSim initModel
        
    match model.WaveSim with
    | None ->
        div [] [
            Button.button
                [ Button.Color IsSuccess; Button.OnClick (fun _ -> startWaveSim()) ]
                [ str "Start waveform simulator" ]
        ]
    | Some simModel ->
        let endWaveSim _ =
            dispatch CloseWaveSimNotification // Copied this, don't know if necessary + it's not doing anything now I think
            dispatch EndWaveSim // End simulation.
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endWaveSim ]
                [ str "Close waveform simulator" ]
            br []; br []
            hr []
            div [] [
                Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "The simulator uses the diagram at the time of pressing the button" ]
            ]
            let displayWaveWithParams = displayWaveform simModel.viewParams 
            let svgList = 
                List.mapi displayWaveWithParams simModel.waves 
                |> List.collect (fun x -> x)
                |> List.append (makeBackground simModel.viewParams)
            
            div [
                Style [OverflowX OverflowOptions.Scroll] 
                ]
                [
                    svg 
                        [ ViewBox "0 0 10 15"; unbox ("width", "100%") ]//should be variables
                        svgList              
                    //right limit should be variable, check lower limit of box
            ]
            
        ]
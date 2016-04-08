namespace PostFlop

module Texture =
  open Hands
  open Options
  open Import

  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    if isFlushDraw then
      let donk = if eo.DonkFlashDraw.IsSome then eo.DonkFlashDraw.Value else eo.Donk
      if eo.CbetFactor = None then
        { Options.CbetFactor = Some 50m; CheckRaise = StackOff; Donk = donk }
      else
        { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = donk }
    else if isFlopFlushDraw && eo.CbetFactor = Some 75m then
      { Options.CbetFactor = Some 100m; CheckRaise = eo.CheckRaise; Donk = eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk }

  let toTurnOptions turnFace isFlushDraw (eo:ExcelOptions) =
    let turn = turnFace |> faceToChar |> string
    if isFlushDraw then
      { Options.CbetFactor = eo.TurnFDCbetFactor; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
    else if eo.TurnFVCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFVCbetFactor; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
    else if eo.TurnFBCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFBCbetFactor; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
    else
      { Options.CbetFactor = None; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
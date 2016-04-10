namespace PostFlop

module Texture =
  open Hands
  open Options
  open Import

  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    if isFlushDraw then
      let donk = if eo.DonkFlashDraw.IsSome then eo.DonkFlashDraw.Value else eo.Donk
      if eo.CbetFactor = Never then
        { Options.CbetFactor = Always 50m; CheckRaise = StackOff; Donk = donk }
      else
        { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = donk }
    else if isFlopFlushDraw && eo.CbetFactor = Always 75m then
      { Options.CbetFactor = Always 100m; CheckRaise = eo.CheckRaise; Donk = eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk }

  let toTurnOptions turnFace isFlush isFlushDraw (eo:ExcelOptions) =
    let turn = turnFace |> faceToChar |> string
    if isFlush then
      { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined }
    else if isFlushDraw && eo.TurnFDCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFDCbetFactor; CheckRaise = OnCheckRaise.AllIn; Donk = OnDonk.Undefined }
    else if eo.TurnFVCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFVCbetFactor; CheckRaise = eo.TurnCheckRaise; Donk = OnDonk.Undefined }
    else if eo.TurnFBCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFBCbetFactor; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined }
    else
      { Options.CbetFactor = Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
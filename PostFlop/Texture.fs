namespace PostFlop

module Texture =
  open Hands
  open Options
  open Import

  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    if isFlushDraw then
      let donk = if eo.DonkFlashDraw.IsSome then eo.DonkFlashDraw.Value else eo.Donk
      let cbetFactor = if eo.CbetFactor = Never then Always 50m else eo.CbetFactor
      { Options.CbetFactor = cbetFactor; CheckRaise = OnCheckRaise.StackOff; Donk = donk }
    else if isFlopFlushDraw && eo.CbetFactor = Always 75m then
      { Options.CbetFactor = Always 100m; CheckRaise = eo.CheckRaise; Donk = eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk }

  let toTurnOptions turnFace isFlush isFlushDraw onDonk monoboard (eo:ExcelOptions) =
    let turn = turnFace |> faceToChar |> string
    if isFlush then
      { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = onDonk }
    else if isFlushDraw && eo.TurnFDCbetCards.Contains(turn) then
      { Options.CbetFactor = Always 75m; CheckRaise = OnCheckRaise.AllIn; Donk = onDonk }
    else if eo.TurnFVCbetCards.Contains(turn) then
      { Options.CbetFactor = (if monoboard < 2 then eo.TurnFVCbetFactor else Always 62.5m) 
        CheckRaise = eo.TurnCheckRaise
        Donk = onDonk }
    else if eo.TurnFBCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFBCbetFactor; CheckRaise = OnCheckRaise.Fold; Donk = onDonk }
    else
      { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = onDonk }
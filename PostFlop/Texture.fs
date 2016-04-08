namespace PostFlop

module Texture =
  open Hands
  open Options
  open Import


  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    if isFlushDraw then
      let donk = if eo.DonkFlashDraw.IsSome then eo.DonkFlashDraw.Value else eo.Donk
      if eo.CbetFactor = None then
        { Options.CbetFactor = Some 50; CheckRaise = StackOff; Donk = donk }
      else
        { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = donk }
    else if isFlopFlushDraw && eo.CbetFactor = Some 75 then
      { Options.CbetFactor = Some 100; CheckRaise = eo.CheckRaise; Donk = eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk }


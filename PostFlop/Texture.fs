namespace PostFlop

module Texture =
  open Hands
  open Options
  open Import


  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    if isFlushDraw then
      if eo.CbetFactor = None then
        { Options.CbetFactor = Some 50; CheckRaise = StackOff; Donk = eo.DonkFlashDraw.Value }
      else
        { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.DonkFlashDraw.Value }
    else if isFlopFlushDraw && eo.CbetFactor = Some 75 then
      { Options.CbetFactor = Some 100; CheckRaise = eo.CheckRaise; Donk = eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk }


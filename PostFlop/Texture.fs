namespace PostFlop

module Texture =
  open Hands
  open Cards.HandValues
  open Cards.Actions
  open Options
  open Import
  open PostFlop.Decision

  let toFlopOptions isFlushDraw isFlopFlushDraw eo =
    let onDonkRaise = function 
      | OnDonk.ForValueStackOff -> OnDonkRaise.StackOff 
      | OnDonk.CallRaisePet -> OnDonkRaise.AllIn
      | _ -> OnDonkRaise.Undefined
    if isFlushDraw then
      let donk = if eo.DonkFlashDraw.IsSome then eo.DonkFlashDraw.Value else eo.Donk
      let cbetFactor = if eo.CbetFactor = Never then Always 50m else eo.CbetFactor
      { Options.CbetFactor = cbetFactor; CheckRaise = OnCheckRaise.StackOff; Donk = donk; DonkRaise = onDonkRaise donk }
    else if isFlopFlushDraw && eo.CbetFactor = Always 75m then
      { Options.CbetFactor = Always 100m; CheckRaise = eo.CheckRaise; Donk = eo.Donk; DonkRaise = onDonkRaise eo.Donk }
    else
      { Options.CbetFactor = eo.CbetFactor; CheckRaise = eo.CheckRaise; Donk = eo.Donk; DonkRaise = onDonkRaise eo.Donk }

  let toTurnOptions (board: Board) value onDonk onDonkRaise monoboard (eo:ExcelOptions) =
    let isFlush = match value.Made with | Flush(_) -> true | _ -> false
    let turn = board.[3].Face |> faceToChar |> string
    if isFlush then
      { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = onDonk; DonkRaise = onDonkRaise }, "FlushFixedRule"
    else if eo.TurnFVCbetCards.Contains(turn) then
      { Options.CbetFactor = (if monoboard < 2 then eo.TurnFVCbetFactor else Always 62.5m) 
        CheckRaise = eo.TurnCheckRaise
        Donk = onDonk
        DonkRaise = onDonkRaise }, "L"
    else if eo.TurnFBCbetCards.Contains(turn) then
      { Options.CbetFactor = eo.TurnFBCbetFactor; CheckRaise = OnCheckRaise.Fold; Donk = onDonk; DonkRaise = onDonkRaise }, "O"
    else
      { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = onDonk; DonkRaise = onDonkRaise }, "-"

  let nutStraightOnBoard s value texture =
    match street s, value.Made with
      | River, Straight(OnBoard) -> 
        let boardStraight = streety 5 0 s.Board
        if boardStraight = Some(14) then
          match texture.Monoboard with
          | 4 -> None
          | 3 -> Some { First = OopDonk.Check; Then = OopOnCBet.CallEQ 32; Scenario = null; Special = [] }
          | _ -> Some { First = OopDonk.AllIn; Then = OopOnCBet.AllIn; Scenario = null; Special = [] }
        else None
      | _ -> None
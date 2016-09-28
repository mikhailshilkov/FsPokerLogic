module Actions

open Preflop
open Cards.Actions

let raiseSize x villainBet stack =
  let rawSize = villainBet |> decimal |> (*) x |> int
  if rawSize > stack * 7 / 10 then Action.AllIn
  else
    let roundedSize = rawSize / 5 * 5
    RaiseToAmount roundedSize

let mapPatternToAction street vb stack (pattern : ActionPattern) =
  let action =
    match pattern with
    | ActionPattern.AllIn -> AllIn
    | ActionPattern.MinRaise -> RaiseToAmount (vb * 2)
    | ActionPattern.RaiseX x | ActionPattern.RaiseBluffX x -> raiseSize x vb stack
    | ActionPattern.Call -> Call
    | ActionPattern.Check -> Check
    | ActionPattern.Fold -> Fold
  let motivation = match pattern with | ActionPattern.RaiseBluffX x -> Some Bluff | _ -> None
  { Action = action; Motivation = motivation; VsVillainBet = vb; Street = street; Source = null }
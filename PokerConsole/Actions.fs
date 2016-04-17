module Actions

open Preflop
open Cards

let raiseSize x villainBet stack =
  let rawSize = villainBet |> decimal |> (*) x |> int
  if rawSize > stack * 7 / 10 then Action.AllIn
  else
    let roundedSize = rawSize / 5 * 5
    RaiseToAmount roundedSize

let mapPatternToAction vb stack (pattern : ActionPattern) =
  match pattern with
  | ActionPattern.AllIn -> AllIn
  | ActionPattern.MinRaise -> MinRaise
  | ActionPattern.RaiseX x -> raiseSize x vb stack
  | ActionPattern.Call -> Call
  | ActionPattern.Check -> Check
  | ActionPattern.Fold -> Fold
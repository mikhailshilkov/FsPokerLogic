namespace PostFlop

open Hands
open Cards.HandValues
open Decision
open Options
open Import
open Monoboard
open Texture
open HandValue

module Facade =
  let defaultArgLazy o (p:'a Lazy) = match o with | Some v -> v | None -> p.Force()

  let decidePostFlop s value special xlFlopTurn xlTurnDonkRiver =
    (match street s with
    | River ->
      let mono = if special.Monoboard >= 4 then monoboardRiver special.Monoboard value.Made else None
      defaultArgLazy mono (lazy (importRiver (fst xlTurnDonkRiver) special value.Made))
    | Turn ->
      let eo = importOptions (fst xlFlopTurn) s.Hand s.Board 
      let turnFace = s.Board.[3].Face
      let turnDonkOption = importTurnDonk (fst xlTurnDonkRiver) special value
      toTurnOptions turnFace (match value.Made with | Flush(_) -> true | _ -> false) (isFlushDrawWith2 s.Hand s.Board) turnDonkOption eo
      |> (if special.Monoboard >= 3 then monoboardTurn special.Monoboard value else id)
    | Flop ->
      let eo = importOptions (fst xlFlopTurn) s.Hand s.Board 
      toFlopOptions (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eo
      |> (if special.Monoboard >= 3 then monoboardFlop value else id)
    )
    |> augmentOptions s
    |> decide s



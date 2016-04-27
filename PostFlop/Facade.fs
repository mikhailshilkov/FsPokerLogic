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
  let decidePostFlop s value special xlFlopTurn xlTurnDonkRiver =
    let eo = importOptions (fst xlFlopTurn) s.Hand s.Board 
    let o =
      match street s with
      | River ->
        let mono = if special.Monoboard >= 4 then monoboardRiver special.Monoboard value.Made else None
        defaultArg mono (importRiver (fst xlTurnDonkRiver) special value.Made)
      | Turn ->
        let mono = if special.Monoboard >= 3 then monoboardTurn special.Monoboard value else None
        defaultArg mono
         (let turnFace = s.Board.[3].Face
          let turnDonkOption = importTurnDonk (fst xlTurnDonkRiver) special value
          toTurnOptions turnFace (match value.Made with | Flush(_) -> true | _ -> false) (isFlushDrawWith2 s.Hand s.Board) turnDonkOption eo)
      | Flop ->
        toFlopOptions (isMonoboard s.Board) (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eo
      |> augmentOptions s
    decide s o



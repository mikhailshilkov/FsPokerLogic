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

  let decidePostFlop s value texture xlFlopTurn xlTurnDonkRiver =
    (match street s with
    | River ->
      let mono = if texture.Monoboard >= 4 then monoboardRiver texture.Monoboard value.Made else None
      defaultArgLazy mono (lazy (importRiver (fst xlTurnDonkRiver) texture value.Made))
    | Turn ->
      let eo = importOptions (fst xlFlopTurn) s.Hand s.Board 
      let turnFace = s.Board.[3].Face
      let turnDonkOption = importTurnDonk (fst xlTurnDonkRiver) texture value
      toTurnOptions turnFace (match value.Made with | Flush(_) -> true | _ -> false) (isFlushDrawWith2 s.Hand s.Board) turnDonkOption eo
      |> (if texture.Monoboard >= 3 then monoboardTurn texture.Monoboard value else id)
    | Flop ->
      let eo = importOptions (fst xlFlopTurn) s.Hand s.Board 
      toFlopOptions (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eo
      |> (if texture.Monoboard >= 3 then monoboardFlop value else id)
    )
    |> augmentOptions s value.Made
    |> decide s

  let decidePostFlopOop s value xlLimpCheck =
    let preFlopPattern =
      if s.Pot = s.BB * 2 then Some "limp and check"
      else None
    match street s, preFlopPattern with
    | Flop, Some p -> 
      importOopFlop (fst xlLimpCheck) p value
      |> Option.bind (decideOop s)
    | _ -> None

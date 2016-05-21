namespace PostFlop

open Cards.HandValues
open Hands
open Options
open Decision

module SpecialRules = 
  let specialRulesOop s o = 
    let rec imp remaining =
      match remaining with
      | CallEQPlusXvsAI dx::rem ->
        match o.Then, s.VillainStack with
        | CallEQ x, 0 -> { o with Then = CallEQ (x + dx) }
        |_ -> imp rem
      | PairedBoard(f, t)::rem ->
        if isPaired s.Board then { o with First = f; Then = t } else imp rem
      | BoardOvercard(f, t)::rem -> 
        let boardBefore = Array.take (s.Board.Length - 1) s.Board
        let latestCardValue = (Array.last s.Board).Face |> faceValue
        if Array.forall (fun x -> faceValue x.Face < latestCardValue) boardBefore then
          { o with First = f; Then = t }
        else imp rem
      | BoardAce f::rem ->
        if (Array.last s.Board).Face = Ace 
          && s.Board |> Array.filter (fun x -> x.Face = Ace) |> Array.length = 1 then 
          { o with First = f; Then = Fold } 
        else imp rem
      | _ -> o
    imp o.Special
namespace PostFlop

open Cards
open Cards.HandValues
open Hands
open Options
open Decision


module SpecialRules = 
  let specialRulesOop s history o = 
    let rec imp remaining =
      match remaining with
      | CallEQPlusXvsAI dx::rem ->
        match o.Then, s.VillainStack with
        | CallEQ x, 0 -> { o with Then = CallEQ (x + dx) }
        |_ -> imp rem
      | PairedBoard(f, t)::rem ->
        if isPaired s.Board then { o with First = f; Then = t } else imp rem
      | BoardOvercard(f, t)::rem -> 
        if isLastBoardCardOvercard s.Board then { o with First = f; Then = t } else imp rem
      | BoardAce(f,t)::rem ->
        if (Array.last s.Board).Face = Ace 
          && s.Board |> Array.filter (fun x -> x.Face = Ace) |> Array.length = 1 then 
          { o with First = f; Then = t } 
        else imp rem
      | CheckCheck(f, t)::rem ->
        if List.tryLast history = Some Action.Check then { o with First = f; Then = t } else imp rem
      | CheckCheckAndBoardOvercard(f, t)::rem ->
        if List.tryLast history = Some Action.Check && isLastBoardCardOvercard s.Board then { o with First = f; Then = t } else imp rem
      | KHighOnPaired::rem ->
        if o.Then = Fold && isPaired s.Board && isXHigh King s.Hand && s.BB <= 30 then 
          { o with Then = CallEQ (if s.BB = 20 then 30 else 25) } 
        else imp rem
      | CheckRaiseBluffOnFlop:: rem ->
        let isBlufyBoard board =
          let bluffyFlops = ["2 5 7"; "2 5 8"; "2 5 9"; "2 6 7"; "2 6 8"; "2 6 9"; "2 6 T"; "2 7 9"; "2 8 J"; "3 3 8"; "3 3 9"; "3 4 7"; "3 4 8"; "3 4 9"; "3 5 6"; "3 5 7"; "3 5 8"; "3 5 9"; "3 5 T"; "3 6 7"; "3 6 8"; "3 6 9"; "3 7 9"; "3 7 T"; "3 7 J"; "3 8 T"; "3 8 J"; "4 4 6"; "4 4 7"; "4 4 8"; "4 4 9"; "4 5 7"; "4 5 8"; "4 5 9"; "4 5 T"; "4 5 J"; "4 6 7"; "4 6 8"; "4 6 9"; "4 6 T"; "4 6 J"; "4 7 9"; "4 7 T"; "4 7 J"; "4 8 T"; "4 8 J"; "4 9 J"; "4 T Q"; "5 5 7"; "5 5 9"; "5 5 T"; "5 6 8"; "5 6 9"; "5 6 T"; "5 6 J"; "5 6 Q"; "5 7 7"; "5 7 8"; "5 7 9"; "5 7 T"; "5 7 J"; "5 7 Q"; "5 8 T"; "5 8 Q"; "5 9 J"; "5 9 Q"; "5 T Q"; "5 J Q"; "6 6 9"; "6 6 T"; "6 6 J"; "6 7 T"; "6 7 Q"; "6 8 T"; "6 8 Q"; "6 T Q"; "6 J Q"; "7 7 T"; "7 7 J"; "7 8 J"; "7 8 Q"; "7 T Q"; "7 J Q"; "8 8 J"; "8 J J"]
          let cards = board |> Seq.take 3 |> Seq.map (fun x -> x.Face) |> Seq.sortBy faceValue |> Seq.map faceToChar
          let key = System.String.Join(" ", cards)
          List.contains key bluffyFlops
        if s.BB = 20 && potPre s = 4 * s.BB && effectiveStackPre s >= 18 && s.VillainBet * 3 = s.Pot
          && (isGutShot s.Hand s.Board || overcards s.Hand s.Board >= 1) && isBlufyBoard s.Board then
          { o with First = Check; Then = RaiseFold } 
        else imp rem
      | [] -> o
    imp o.Special
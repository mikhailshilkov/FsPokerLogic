namespace Player

open Import
open Preflop
open Hands
open Recognition.ScreenRecognition

module Decide =

  let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
  let rulesIP = importRuleFromExcel importRulesIP fileNameIP |> List.ofSeq
  let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
  let rulesOOP = importRuleFromExcel importRulesOOP fileNameOOP |> List.ofSeq
  let rules = Seq.concat [|rulesIP;rulesOOP|]
  let decidePre stack = decideOnRules rules stack

  let parseFullHand (s : string) =
    let card1 = parseFace s.[0]
    let card2 = parseFace s.[2]
  
    let sameSuit = 
      if card1 = card2 then false
      else s.[1] = s.[3]

    { Card1 = card1
      Card2 = card2
      SameSuit = sameSuit }

  let understandHistory screen =
    let raise bet bb = 
      let b = (bet |> decimal) / (bb |> decimal)
      Raise(b, b)
    let villainAllIn = screen.VillainStack = Some 0
    match screen.Button, screen.Blinds, screen.VillainBet, screen.HeroBet with
    | Hero, Some {BB = bb; SB = sb}, Some vb, Some hb when vb <= bb && sb = hb -> []
    | Hero, Some {BB = bb}, Some vb, Some hb when vb > bb && bb = hb -> [Limp; raise vb bb]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise hb bb; RaiseAllIn]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise hb bb; raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when vb = bb && hb = bb -> [Limp]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb && villainAllIn -> [RaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb -> [raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb && villainAllIn -> [Limp; raise hb bb; RaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb -> [Limp; raise hb bb; raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise ((hb + bb) / 2) bb; raise hb bb; RaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise ((hb + bb) / 2) bb; raise hb bb; raise vb bb]
    | _ -> failwith "History is not clear"

  let decide' screen : Action option =
    match screen.HeroStack, screen.HeroBet, screen.VillainStack, screen.VillainBet, screen.Blinds with
    | Some hs, Some hb, Some vs, Some vb, Some b -> 
      let stack = min (hs + hb) (vs + vb)
      let effectiveStack = decimal stack / decimal b.BB
      let fullHand = parseFullHand screen.HeroHand
      let history = understandHistory screen
      decidePre effectiveStack history fullHand
    | _ -> None


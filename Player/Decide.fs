namespace Player

open Akka.FSharp
open Click
open Import
open Preflop
open System.Drawing
open Hands
open Recognition.ScreenRecognition

module Decide =
  open Interaction

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

  type DecisionMessage = {
    WindowTitle: string
    TableName: string
    Screen: Screen
    Bitmap: Bitmap
  }

  let mapAction action buttons : ClickAction[] =
    let findButton names =
      buttons |> Array.tryFind (fun x -> Seq.exists (fun y -> x.Name = y) names)
    let button =
      match action with
      | Fold -> ["Check"; "Fold"]
      | Check -> ["Check"]
      | Call -> ["Call"; "AllIn"]
      | MinRaise -> ["RaiseTo"; "Bet"]
      | RaiseX _ -> ["RaiseTo"; "Bet"]
      | AllIn -> ["RaiseTo"; "Bet"]
      |> findButton

    match (action, button) with
    | (AllIn, Some b) -> [|Click(368, 389, 42, 7); Click(b.Region)|]
    | (RaiseX x, Some b) -> [| Amount(x); Click(b.Region)|]
    | (_, Some b) -> [|Click(b.Region)|]
    | (_, None) -> failwith "Could not find an appropriate button"

  let actor2 clickerRef =
    let mutable lastScreen = None
    let imp (mailbox : Actor<'a>) msg =
      let screen = msg.Screen
      match lastScreen with
      | Some s when s = screen -> ()
      | _ ->
        lastScreen <- Some screen
        print screen |> Seq.iter (printfn "%s: %s" "Hand")
        let decision = decide' screen
        match decision with
        | Some d ->
          printfn "Decision is: %A" d
          let action = mapAction d screen.Actions
          printfn "Action is: %A" action
          clickerRef <! { WindowTitle = msg.WindowTitle; Clicks = action }
        | None ->
          printfn "Could not make a decision, dumping the screenshot..."
          Dumper.SaveBitmap(msg.Bitmap, msg.TableName)
    imp

  let decideActor clickerRef (mailbox : Actor<DecisionMessage>) =
    let rec imp lastScreen =
      actor {
        let! msg = mailbox.Receive()

        let screen = msg.Screen
        match lastScreen with
        | Some s when s = screen -> ()
        | _ ->
          print screen |> Seq.iter (printfn "%s: %s" "Hand")
          let decision = decide' screen
          match decision with
          | Some d ->
            printfn "Decision is: %A" d
            let action = mapAction d screen.Actions
            printfn "Action is: %A" action
            clickerRef <! { WindowTitle = msg.WindowTitle; Clicks = action }
          | None ->
            printfn "Could not make a decision, dumping the screenshot..."
            Dumper.SaveBitmap(msg.Bitmap, msg.TableName)

        return! imp (Some screen)
      }

    imp None
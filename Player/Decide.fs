namespace Player

open Akka.FSharp
open Cards
open Click
open Import
open Preflop
open System.Drawing
open Hands
open Cards.HandValues
open Actions
open Recognition.ScreenRecognition
open PostFlop.HandValue
open PostFlop.Decision
open PostFlop.Facade

module Decide =
  open Interaction

  let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
  let rulesIP = importRuleFromExcel (importRulesByStack importRulesIP) fileNameIP |> List.ofSeq
  let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
  let rulesOOP = importRuleFromExcel (importRulesByStack importRulesOOP) fileNameOOP |> List.ofSeq
  let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
  let rulesAdvancedOOP = importRuleFromExcel importOopAdvanced fileNameAdvancedOOP |> List.ofSeq
  let rules = Seq.concat [|rulesIP;rulesAdvancedOOP;rulesOOP|]
  let decidePre stack odds = decideOnRules rules stack odds

  let understandHistory (screen: Screen) =
    let raise bet bb = 
      let b = (bet |> decimal) / (bb |> decimal)
      WasRaise(b)
    let villainAllIn = screen.VillainStack = Some 0
    match screen.Button, screen.Blinds, screen.VillainBet, screen.HeroBet with
    | Hero, Some {BB = bb; SB = sb}, Some vb, Some hb when vb <= bb && sb = hb -> []
    | Hero, Some {BB = bb}, Some vb, Some hb when vb > bb && bb = hb -> [WasLimp; raise vb bb]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise hb bb; WasRaiseAllIn]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise hb bb; raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when vb = bb && hb = bb -> [WasLimp]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb && villainAllIn -> [WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb -> [raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb && villainAllIn -> [WasLimp; raise hb bb; WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb -> [WasLimp; raise hb bb; raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise ((hb + bb) / 2) bb; raise hb bb; WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise ((hb + bb) / 2) bb; raise hb bb; raise vb bb]
    | _ -> failwith "History is not clear"

  let decide' xlFlopTurn xlTurnDonkRiver xlPostFlopOop (screen: Screen) history: Action option =
    let decidePre (screen: Screen): Action option =
      match screen.HeroStack, screen.HeroBet, screen.VillainStack, screen.VillainBet, screen.Blinds with
      | Some hs, Some hb, Some vs, Some vb, Some b -> 
        let stack = min (hs + hb) (vs + vb)
        let effectiveStack = decimal stack / decimal b.BB
        let callSize = min (vb - hb) hs
        let potOdds = (callSize |> decimal) * 100m / (vb + hb + callSize |> decimal) |> ceil |> int
        let openRaise = if b.BB >= 20 then 40m else if b.BB >= 16 then 34m else 31m
        let fullHand = parseFullHand screen.HeroHand
        let history = understandHistory screen
        let actionPattern = decidePre effectiveStack potOdds openRaise history fullHand
        Option.map (mapPatternToAction vb stack) actionPattern  
      | _ -> None
    let decidePost (screen: Screen) =
      match screen.TotalPot, screen.HeroStack, screen.VillainStack, screen.Blinds with
      | Some tp, Some hs, Some vs, Some b -> 
        let suitedHand = screen.HeroHand |> parseSuitedHand
        let board = screen.Board |> parseBoard
        let value = handValueWithDraws suitedHand board
        let special = boardTexture board
        let vb = defaultArg screen.VillainBet 0
        let hb = defaultArg screen.HeroBet 0
        let s = { Hand = suitedHand; Board = board; Pot = tp; VillainStack = vs; HeroStack = hs; VillainBet = vb; HeroBet = hb; BB = b.BB }
        if screen.Button = Hero then 
          decidePostFlop s value special xlFlopTurn xlTurnDonkRiver
        else
          decidePostFlopOop history s value special xlPostFlopOop
      | _ -> None

    match screen.Sitout, screen.Board with
    | Villain, _ -> Some Action.MinRaise
    | Hero, _ -> Some Action.SitBack
    | _, null -> decidePre screen
    | _, _ -> decidePost screen

  type DecisionMessage = {
    WindowTitle: string
    TableName: string
    Screen: Screen
    Bitmap: Bitmap
  }

  type DecisionState = {
    LastScreen: Screen
    PreviousActions: Action list
  }

  let mapAction action buttons : ClickAction[] =
    let findButton names =
      names 
      |> List.choose (fun x -> Seq.tryFind (fun y -> x = y.Name) buttons)
      |> List.tryHead
    let button =
      match action with
      | Action.Fold -> ["Check"; "Fold"]
      | Action.Check -> ["Check"]
      | Action.Call -> ["Call"; "AllIn"]
      | Action.MinRaise -> ["RaiseTo"; "Bet"; "AllIn"; "Call"]
      | Action.RaiseToAmount _ -> ["RaiseTo"; "Bet"; "AllIn"; "Call"]
      | Action.AllIn -> ["AllIn"; "RaiseTo"; "Bet"; "Call"]
      | Action.SitBack -> ["SitBack"]
      |> findButton

    match (action, button) with
    | (Action.AllIn, Some b) -> [|Click(368, 389, 42, 7); Click(b.Region)|]
    | (Action.RaiseToAmount x, Some b) -> [| Click(599, 407, 18, 9); Amount(x); Click(b.Region)|]
    | (_, Some b) -> [|Click(b.Region)|]
    | (_, None) -> failwith "Could not find an appropriate button"

  let decisionActor xlFlopTurn xlTurnDonk xlPostFlopOop msg (state:DecisionState option) =
    let pushAction state action reset =
      match state, action with
      | Some s, Some a -> if reset then [a] else List.append s.PreviousActions [a]
      | Some s, None -> s.PreviousActions
      | None, Some a -> [a]
      | None, None -> []

    let screen = msg.Screen
    match state with
    | Some s when s.LastScreen = screen -> (None, state)
    | _ ->
      print screen |> List.iter (printfn "%s: %s" "Hand")
      let isPre = System.String.IsNullOrEmpty screen.Board
      let history = if isPre then [] else Option.map (fun s -> s.PreviousActions) state |> defaultArg <| []
      history |> List.iter (printfn "History: %A")

      let decision = decide' xlFlopTurn xlTurnDonk xlPostFlopOop screen history
      let newState = Some { LastScreen = screen; PreviousActions = pushAction state decision isPre }
      match decision with
      | Some d ->
        printfn "Decision is: %A" d
        let action = mapAction d screen.Actions
        printfn "Action is: %A" action
        let outMsg = { WindowTitle = msg.WindowTitle; Clicks = action; IsInstant = screen.Sitout <> Unknown; Screen = screen }
        (Some outMsg, newState)
      | None ->
        printfn "Could not make a decision, dumping the screenshot..."
        Dumper.SaveBitmap(msg.Bitmap, msg.TableName)
        (None, newState)
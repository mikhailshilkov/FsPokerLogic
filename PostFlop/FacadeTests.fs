module FacadeTests

open Excel.Import
open Xunit
open Cards.Actions
open Hands
open Cards.HandValues
open PostFlop.Options
open PostFlop.Decision
open PostFlop.HandValue
open PostFlop.Facade
open PostFlop.Import

let defaultFlop = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}|]; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultValue = { Made = Nothing; FD = NoFD; FD2 = NoFD; SD = NoSD }

let nml l = l |> List.map (fun nm -> (nm, None))

[<Fact>]
let ``pickOopSheet returns limp coorectly for single item`` () =
  let actual = pickOopSheet (nml [Action.Check]) defaultFlop
  Assert.Equal(Some "limp and check", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet returns limp coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.Check; Action.Call; Action.RaiseToAmount 100]) defaultFlop
  Assert.Equal(Some "limp and check", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for single items`` () =
  let actual = pickOopSheet (nml [Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet returns skips SitBack`` () =
  let actual = pickOopSheet (nml [Action.SitBack; Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.Call; Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for single item`` () =
  let actual = pickOopSheet (nml [Action.RaiseToAmount 60]) defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", fst actual)
  Assert.Equal(false, snd actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.RaiseToAmount 60; Action.RaiseToAmount 100]) defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", fst actual)
  Assert.Equal(false, snd actual)

[<Fact>]
let ``pickOopSheet returns raise FB coorectly for single item`` () =
  let actual = pickOopSheet [(Action.RaiseToAmount 60, Some Bluff)] defaultFlop
  Assert.Equal(Some "hero raise FB vs limp", fst actual)
  Assert.Equal(false, snd actual)

[<Fact>]
let ``pickOopSheet falls back to limp for empty history and small pot`` () =
  let s = {defaultFlop with VillainBet = 30; Pot = 70}
  let actual = pickOopSheet [] s
  Assert.Equal(Some "limp and check", fst actual)
  Assert.Equal(true, snd actual)

[<Fact>]
let ``pickOopSheet falls back to call for empty history and bigger pot`` () =
  let s = {defaultFlop with VillainBet = 30; Pot = 110}
  let actual = pickOopSheet [] s
  Assert.Equal(Some "hero call raise pre", fst actual)
  Assert.Equal(true, snd actual)

let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
let adxl = useExcel fileNameAdvancedOOP
let bluffy = 
  (importFlopList "bluffy hero ch-r flop vs limp" adxl.Workbook,
    importFlopList "bluffy hero ch-r flop vs minr" adxl.Workbook,
    importFlopList "bluffy overtaking, vill ch b fl" adxl.Workbook)

let riverBetSizing = 
  ([{ MinPotSize = 0; MaxPotSize = 500; MinAllInPercentage = 40; MaxAllInPercentage = 70; MinChipsLeft = 50; BetSize = 60 }]
  ,[{ MinPotSize = 0; MaxPotSize = 500; BetSize = 60; ThinBetSize = 50 }]
  ,[{ MinPotSize = 0; MaxPotSize = 500; F1RRRatio = 2m; RTVRatio = 2.5m }])

let testPostFlopMotivatedExt h s mono test =
  let v = handValueWithDraws s.Hand s.Board
  let t = { Streety = false; DoublePaired = false; ThreeOfKind = false; FourOfKind = false; Monoboard = mono }

  let fileNameOop = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
  use xlOop = useExcel fileNameOop
  let fileNameTricky = System.IO.Directory.GetCurrentDirectory() + @"\tricky.xlsx"
  use xlTricky = useExcel fileNameTricky
  let actual = decidePostFlopOop h s v t xlOop.Workbook xlTricky.Workbook bluffy ((fun _ -> false), (fun _ -> false)) riverBetSizing
  test actual

let testPostFlopMotivated h s mono expected =
  testPostFlopMotivatedExt h s mono (fun x -> Assert.Equal(expected, x.Value.Action))

let testPostFlopExt h s mono test =
  let mh = h |> List.map (notMotivated Flop s.VillainBet)
  testPostFlopMotivatedExt mh s mono test

let testPostFlop h s mono expected =
  let mh = h |> List.map (notMotivated Flop s.VillainBet)
  testPostFlopMotivated mh s mono expected

[<Fact>]
let ``decidePostFlop bet on turn`` () =
  let s = { Hand = parseSuitedHand "TcQh"; Board = parseBoard "TsTd7sQs"; Pot = 240; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 60; Action.RaiseToAmount 60] s 3 (Action.RaiseToAmount 150)

[<Fact>]
let ``decidePostFlop folds flop on 3 bet with 2nd + GS`` () =
  let s = { Hand = parseSuitedHand "8s7h"; Board = parseBoard "JcTh8d"; Pot = 570; VillainStack = 0; HeroStack = 300; VillainBet = 255; HeroBet = 90; BB = 20 }
  testPostFlop [Action.Check; Action.RaiseToAmount 90] s 0 (Action.Fold)

[<Fact>]
let ``decidePostFlop folds flop on 3 bet with OE`` () =
  let s = { Hand = parseSuitedHand "5h3d"; Board = parseBoard "8h2c4s"; Pot = 570; VillainStack = 0; HeroStack = 130; VillainBet = 685; HeroBet = 125; BB = 20 }
  testPostFlop [Action.Check; Action.RaiseToAmount 90] s 0 (Action.Fold)

[<Fact>]
let ``decidePostFlop bov#55% special rule on turn`` () =
  let s = { Hand = parseSuitedHand "7s8s"; Board = parseBoard "5s7c3hTd"; Pot = 200; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 40; Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 110)

[<Fact>]
let ``decidePostFlop bov#55% special rule on turn 2`` () =
  let s = { Hand = parseSuitedHand "Jh9c"; Board = parseBoard "5sJc5dKs"; Pot = 240; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 60; Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 130)

[<Fact>]
let ``decidePostFlop A/f special rule on turn`` () =
  let s = { Hand = parseSuitedHand "7sTs"; Board = parseBoard "8cKsJdAc"; Pot = 200; VillainStack = 300; HeroStack = 600; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 40; Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 135)

[<Fact>]
let ``decidePostFlop 61 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "Td7s"; Board = parseBoard "5hTh9sAs"; Pot = 220; VillainStack = 380; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 30 }
  testPostFlop [Action.Check; Action.Check; Action.RaiseToAmount 80] s 0 (Action.RaiseToAmount 130)

[<Fact>]
let ``decidePostFlop 61 special rule on turn 2`` () =
  let s = { Hand = parseSuitedHand "7d5s"; Board = parseBoard "3d7h2cAh"; Pot = 80; VillainStack = 480; HeroStack = 480; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Check; Action.Check; Action.Call] s 0 (Action.RaiseToAmount 50)

[<Fact>]
let ``decidePostFlop 7 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "8d9h"; Board = parseBoard "9cQd8s6h"; Pot = 100; VillainStack = 450; HeroStack = 450; VillainBet = 0; HeroBet = 0; BB = 20 }
  let h = [ notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  testPostFlopMotivated h s 0 (Action.RaiseToAmount 75)

[<Fact>]
let ``decidePostFlop 5 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "QsKh"; Board = parseBoard "TdQc9d5h"; Pot = 100; VillainStack = 380; HeroStack = 520; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 75)

[<Fact>]
let ``decidePostFlop give up turn after bluffy flop check raise`` () =
  let s = { Hand = parseSuitedHand "4hQc"; Board = parseBoard "Ts7c5d5h"; Pot = 300; VillainStack = 350; HeroStack = 350; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check; Action.RaiseToAmount 110] s 0 Action.Check

[<Fact>]
let ``decidePostFlop cbet flop after raising limp pre`` () =
  let s = { Hand = parseSuitedHand "Qc9d"; Board = parseBoard "TdKs9c"; Pot = 120; VillainStack = 440; HeroStack = 440; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 60)

[<Fact>]
let ``decidePostFlop does not float with 2FD on flop`` () =
  let s = { Hand = parseSuitedHand "Qh3h"; Board = parseBoard "2hAh3s"; Pot = 120; VillainStack = 530; HeroStack = 350; VillainBet = 40; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 110)

[<Fact>]
let ``decidePostFlop all in on turn after bluffy check/raise on flop`` () =
  let s = { Hand = parseSuitedHand "6h9h"; Board = parseBoard "Jd7h3d5s"; Pot = 300; VillainStack = 340; HeroStack = 340; VillainBet = 0; HeroBet = 0; BB = 20 }
  let h = [ notMotivated PreFlop 40 Action.Call 
            notMotivated Flop 0 Action.Check 
            bluff Flop 40 110]
  testPostFlopMotivated h s 0 Action.AllIn


[<Fact>]
let ``decidePostFlop bet half pot on turn with FD+2nd pair`` () =
  let s = { Hand = parseSuitedHand "Ts8s"; Board = parseBoard "2dAs2sTc"; Pot = 80; VillainStack = 350; HeroStack = 350; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 40)

[<Fact>]
let ``decidePostFlop bet turn for value after 3bet pf and bet flop`` () =
  let s = { Hand = parseSuitedHand "JhJs"; Board = parseBoard "3c2h6d7c"; Pot = 80; VillainStack = 300; HeroStack = 300; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 100; Action.RaiseToAmount 100] s 0 Action.AllIn

[<Fact>]
let ``decidePostFlop cbet flop after bluffy 3bet pf`` () =
  let s = { Hand = parseSuitedHand "5c4d"; Board = parseBoard "AhKcJh"; Pot = 200; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [bluff Flop 40 110] s 0 (Action.RaiseToAmount 100)

[<Fact>]
let ``decidePostFlop does not bluffy overtake turn after wrong flop`` () =
  let s = { Hand = parseSuitedHand "5c8c"; Board = parseBoard "7d4s3hTs"; Pot = 200; VillainStack = 610; HeroStack = 290; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 Action.Check

[<Fact>]
let ``decidePostFlop overtake turn on main rule 2`` () =
  let s = { Hand = parseSuitedHand "3d8h"; Board = parseBoard "7d3hTd6h"; Pot = 60; VillainStack = 590; HeroStack = 350; VillainBet = 0; HeroBet = 0; BB = 30 }
  testPostFlop [Action.Check; Action.Check] s 0 (Action.RaiseToAmount 45)

[<Fact>]
let ``decidePostFlop bet river after overtake turn on main rule 2`` () =
  let s = { Hand = parseSuitedHand "3sKh"; Board = parseBoard "3dJc9d2sAd"; Pot = 100; VillainStack = 340; HeroStack = 560; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Check; Action.Check; Action.RaiseToAmount 30] s 0 (Action.RaiseToAmount 50)

[<Fact>]
let ``decidePostFlop bet river after overtake turn on main rule 2.2`` () =
  let s = { Hand = parseSuitedHand "Qc4h"; Board = parseBoard "8h3h4dKsJh"; Pot = 100; VillainStack = 390; HeroStack = 610; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Check; Action.Check; Action.RaiseToAmount 30] s 0 (Action.RaiseToAmount 50)

[<Fact>]
let ``2`` () =
  let s = { Hand = parseSuitedHand "Qc4h"; Board = parseBoard "8h3h4dKsJh"; Pot = 100; VillainStack = 390; HeroStack = 610; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Check; Action.Check; Action.RaiseToAmount 30] s 0 (Action.RaiseToAmount 50)

[<Fact>]
let ``8`` () =
  let s = { Hand = parseSuitedHand "5s7c"; Board = parseBoard "4cAdQc"; Pot = 200; VillainStack = 370; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [bluff Flop 40 100] s 0 (Action.RaiseToAmount 100)

[<Fact>]
let ``13`` () =
  let s = { Hand = parseSuitedHand "9sJh"; Board = parseBoard "9cKs7c5d"; Pot = 272; VillainStack = 308; HeroStack = 420; VillainBet = 112; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check; Action.Call; Action.Check] s 0 Action.Fold

[<Fact>]
let ``14f`` () =
  let s = { Hand = parseSuitedHand "4hJh"; Board = parseBoard "7d5s8s"; Pot = 120; VillainStack = 290; HeroStack = 590; VillainBet = 40; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 110)

[<Fact>]
let ``14`` () =
  let s = { Hand = parseSuitedHand "4hJh"; Board = parseBoard "7d5s8s6d"; Pot = 300; VillainStack = 220; HeroStack = 480; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check 
    bluff Flop 40 110] s 0 Action.AllIn

[<Fact>]
let ``Turn OOP bet returns appropriate scenario name`` () =
  let s = { Hand = parseSuitedHand "4h5c"; Board = parseBoard "7d2s6dKs"; Pot = 80; VillainStack = 320; HeroStack = 580; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivatedExt [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check] s 0 (fun actual -> Assert.Equal(Some(Scenario "r8"), actual.Value.Motivation))

[<Fact>]
let ``River OOP bet is made based on turn scenario`` () =
  let s = { Hand = parseSuitedHand "4h5c"; Board = parseBoard "7d2s6dKsTh"; Pot = 160; VillainStack = 280; HeroStack = 440; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [
    notMotivated PreFlop 40 Action.Call;
    notMotivated Flop 0 Action.Check 
    scenario Turn 0 (Action.RaiseToAmount 40) "r8"] s 0 (Action.RaiseToAmount 95)

[<Fact>]
let ``Flop OOP: float call`` () =
  let s = { Hand = parseSuitedHand "QdJh"; Board = parseBoard "2s7c2h"; Pot = 120; VillainStack = 330; HeroStack = 590; VillainBet = 40; HeroBet = 0; BB = 20 }
  testPostFlopExt [Action.Call; Action.Check] s 0 
    (fun actual -> Assert.Equal(Action.Call, actual.Value.Action); Assert.Equal(Some(Float BluffFloat), actual.Value.Motivation))

[<Fact>]
let ``Turn OOP: float call for bluffy`` () =
  let s = { Hand = parseSuitedHand "Ad6h"; Board = parseBoard "2s7c3h6h"; Pot = 205; VillainStack = 285; HeroStack = 540; VillainBet = 45; HeroBet = 0; BB = 20 }
  testPostFlopMotivatedExt [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    floatBluff Flop 40
    notMotivated Turn 0 Action.Check] s 0 
    (fun actual -> Assert.Equal(Action.Call, actual.Value.Action); Assert.Equal(Some(Float BluffFloat), actual.Value.Motivation))

[<Fact>]
let ``River OOP: bet after floats`` () =
  let s = { Hand = parseSuitedHand "QdQh"; Board = parseBoard "2s7c2hJhTc"; Pot = 250; VillainStack = 285; HeroStack = 495; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [
    notMotivated PreFlop 40 Action.Call
    notMotivated Flop 0 Action.Check
    floatBluff Flop 40
    notMotivated Turn 0 Action.Check
    floatBluff Turn 45] s 0 (Action.RaiseToAmount 150)

[<Fact>]
let ``River OOP: bet float after check-check on turn`` () =
  let s = { Hand = parseSuitedHand "QhKc"; Board = parseBoard "Ts5c3hQs7d"; Pot = 144; VillainStack = 418; HeroStack = 438; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlopMotivated [
    notMotivated PreFlop 40 Action.Call
    notMotivated Flop 0 Action.Check
    floatBluff Flop 32
    floatBluffCheck Turn] s 0 (Action.RaiseToAmount 85)


let fileNameFlopTurn = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
let fileNameHandStrength = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
let fileNameTricky = System.IO.Directory.GetCurrentDirectory() + @"\tricky.xlsx"

let testIPext s h test =
  use xlFlopTurn = useExcel fileNameFlopTurn
  use xlHandStrength = useExcel fileNameHandStrength
  use xlTricky = useExcel fileNameTricky
  let riverHistoryPatterns = importRiverPatterns xlHandStrength.Workbook
  let v = handValueWithDraws s.Hand s.Board
  let t = boardTexture s.Board
  let actual = decidePostFlop h s v t xlFlopTurn.Workbook xlHandStrength.Workbook xlTricky.Workbook riverBetSizing riverHistoryPatterns
  test actual

let testIP s h expected =
  testIPext s h (fun a -> Assert.Equal(expected |> Some, a |> Option.map (fun m -> m.Action)))

let testIPm s h e m =
  let test actual =
    Assert.Equal(e |> Some, actual |> Option.map (fun m -> m.Action))
    Assert.Equal(m |> Some, actual |> Option.bind (fun m -> m.Motivation))
  testIPext s h test

let testIPSource s h a source =
  let test actual =
    Assert.Equal(a |> Some, actual |> Option.map (fun m -> m.Action))
    Assert.Equal(source |> Some, actual |> Option.map (fun m -> m.Source))
  testIPext s h test

[<Fact>]
let ``decideFlopCbetMixup check on flop`` () =
  use xlHandStrength = useExcel fileNameHandStrength
  let s = { Hand = parseSuitedHand "2cQh"; Board = parseBoard "2s2dAs"; Pot = 80; VillainStack = 460; HeroStack = 460; VillainBet = 0; HeroBet = 0; BB = 20 }
  let h = [notMotivated PreFlop 20 (RaiseToAmount 40)]
  let actual = decideFlopCbetMixup xlHandStrength.Workbook h s defaultValue ()
  Assert.Equal(Action.Check |> Some, actual |> Option.map (fun x -> x.Action))
  Assert.Equal("HandStrength -> cbet mix up -> E14", actual.Value.Source)

[<Fact>]
let ``decideFlopCbetMixup call check/raise on flop`` () =
  use xlHandStrength = useExcel fileNameHandStrength
  let s = { Hand = parseSuitedHand "KcTh"; Board = parseBoard "TsQdQs"; Pot = 220; VillainStack = 360; HeroStack = 420; VillainBet = 100; HeroBet = 40; BB = 20 }
  let h = [
    notMotivated PreFlop 20 (RaiseToAmount 40)
    { notMotivated Flop 0 (RaiseToAmount 40) with Source = "HandStrength -> cbet mix up -> E14" }]
  let actual = decideFlopCbetMixup xlHandStrength.Workbook h s (handValueWithDraws s.Hand s.Board) ()
  Assert.Equal(Action.Call |> Some, actual |> Option.map (fun x -> x.Action))
  Assert.Equal("HandStrength -> flop hand strength -> B14", actual.Value.Source)

[<Fact>]
let ``decidePostFlopIP cbet flush draw on turn`` () =
  let s = { Hand = parseSuitedHand "Jd8d"; Board = parseBoard "Th6d4dQc"; Pot = 120; VillainStack = 390; HeroStack = 450; VillainBet = 0; HeroBet = 0; BB = 20 }
  testIP s [] (Action.RaiseToAmount 90)

[<Fact>]
let ``decidePostFlopIP call 4bet on flop with stackoff`` () =
  let s = { Hand = parseSuitedHand "7sJh"; Board = parseBoard "3h7h6c"; Pot = 765; VillainStack = 0; HeroStack = 120; VillainBet = 460; HeroBet = 225; BB = 20 }
  let history = [
    notMotivated PreFlop 20 (RaiseToAmount 40)
    notMotivated Flop 0 (RaiseToAmount 40)
    notMotivated Flop 100 (RaiseToAmount 225)]
  testIP s history Action.AllIn

[<Fact>]
let ``Flop IP: float call`` () =
  let s = { Hand = parseSuitedHand "7d2d"; Board = parseBoard "2h3h4c"; Pot = 60; VillainStack = 450; HeroStack = 470; VillainBet = 20; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call]
  testIPm s history Action.Call (Float(BluffFloat))

[<Fact>]
let ``Turn IP: float call donk`` () =
  let s = { Hand = parseSuitedHand "6d4d"; Board = parseBoard "2h3h4c7c"; Pot = 120; VillainStack = 410; HeroStack = 450; VillainBet = 40; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatValue Flop 20]
  testIPm s history Action.Call (Float(ValueFloat))

[<Fact>]
let ``Turn IP: bet after bluff float call on flop`` () =
  let s = { Hand = parseSuitedHand "KdJh"; Board = parseBoard "2h3h4cAc"; Pot = 80; VillainStack = 460; HeroStack = 460; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 20]
  testIPm s history (Action.RaiseToAmount 50) (Scenario("r9"))

[<Fact>]
let ``Turn IP: bet turn booster`` () =
  let s = { Hand = parseSuitedHand "QdJh"; Board = parseBoard "2hThJc4c"; Pot = 160; VillainStack = 420; HeroStack = 420; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 (Action.RaiseToAmount 40)]
  testIP s history (Action.RaiseToAmount 100)

[<Theory>]
[<InlineData("HandStrength -> postflop IP turn booster -> C13")>]
[<InlineData("PostflopIP -> O")>]
let ``Turn IP: turn booster calls bet after bluffy bet based on turn booster or Postflop IP O column`` previousMotivation =
  let s = { Hand = parseSuitedHand "QdTh"; Board = parseBoard "2h3h6cJc"; Pot = 500; VillainStack = 220; HeroStack = 280; VillainBet = 120; HeroBet = 60; BB = 20 }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 50); notMotivated Flop 0 (Action.RaiseToAmount 110); 
    { Action = RaiseToAmount 60; Motivation = Some Bluff; VsVillainBet = 0; Street = Turn; Source = previousMotivation }]
  testIP s history Action.Call

[<Fact>]
let ``Turn IP: uses proper non-donk cell of tricky float after check raise`` () =
  let s = { Hand = parseSuitedHand "Th9h"; Board = parseBoard "3s5d6h4c"; Pot = 600; VillainStack = 0; HeroStack = 400; VillainBet = 470; HeroBet = 50; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 20; scenario Turn 0 (Action.RaiseToAmount 50) "r9"]
  testIPSource s history Action.Fold "tricky -> float IP -> P14"

[<Fact>]
let ``River IP: float raise donk`` () =
  let s = { Hand = parseSuitedHand "7d2d"; Board = parseBoard "2h3h4c7c7h"; Pot = 240; VillainStack = 330; HeroStack = 410; VillainBet = 80; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatValue Flop 20; floatValue Turn 40]
  testIP s history (Action.RaiseToAmount 210)

[<Fact>]
let ``River IP: bet after bluff float call on flop`` () =
  let s = { Hand = parseSuitedHand "KdJh"; Board = parseBoard "2h3h4cAcJd"; Pot = 160; VillainStack = 420; HeroStack = 420; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 20; floatBluff Turn 40]
  testIP s history (Action.RaiseToAmount 65)

[<Fact>]
let ``River IP float bet is made based on turn scenario`` () =
  let s = { Hand = parseSuitedHand "KdJh"; Board = parseBoard "2h3hAc4cTd"; Pot = 160; VillainStack = 420; HeroStack = 420; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 20; scenario Turn 0 (Action.RaiseToAmount 40) "r9"]
  testIP s history (Action.RaiseToAmount 95)

[<Fact>]
let ``River IP: sample decision`` () =
  let s = { Hand = parseSuitedHand "KdJh"; Board = parseBoard "2h3h4c4cJd"; Pot = 160; VillainStack = 420; HeroStack = 420; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 (Action.RaiseToAmount 40); notMotivated Flop 0 (Action.RaiseToAmount 40); notMotivated Turn 0 Action.Check]
  testIP s history (Action.RaiseToAmount 100)

[<Fact>]
let ``River IP: sample decision vs villain bet`` () =
  let s = { Hand = parseSuitedHand "KdJh"; Board = parseBoard "2h3h4c4cJd"; Pot = 240; VillainStack = 340; HeroStack = 420; VillainBet = 80; HeroBet = 0; BB = 20 }
  let history = [notMotivated PreFlop 20 (Action.RaiseToAmount 40); notMotivated Flop 0 (Action.RaiseToAmount 40); notMotivated Turn 0 Action.Check]
  testIP s history (Action.RaiseToAmount 210)

[<Fact>]
let ``River IP: picks correct sheet and column vs check-raise`` () =
  let s = { Hand = parseSuitedHand "AhJs"; Board = parseBoard "2hQd2c5h5s"; Pot = 840; VillainStack = 0; HeroStack = 160; VillainBet = 300; HeroBet = 180; BB = 20 }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 40)
    notMotivated Flop 0 (Action.RaiseToAmount 40)
    notMotivated Turn 0 (Action.RaiseToAmount 100)
    notMotivated River 0 (Action.RaiseToAmount 180)]
  testIPSource s history Action.Fold "river - villain check -> Z13"

[<Fact>]
let ``River IP bet is made based on turn scenario`` () =
  let s = { Hand = parseSuitedHand "4h5c"; Board = parseBoard "7d2s6dKsTh"; Pot = 160; VillainStack = 280; HeroStack = 440; VillainBet = 0; HeroBet = 0; BB = 20 }
  let history = [
    notMotivated PreFlop 40 Action.Call;
    notMotivated Flop 40 Action.Call
    scenario Turn 0 (Action.RaiseToAmount 40) "r8"]
  testIPSource s history (Action.RaiseToAmount 95) "river - villain check -> L10"

adxl.Dispose()
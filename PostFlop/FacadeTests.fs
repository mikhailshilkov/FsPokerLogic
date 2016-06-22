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

let nml l = l |> List.map (fun nm -> (nm, None))

[<Fact>]
let ``pickOopSheet returns limp coorectly for single item`` () =
  let actual = pickOopSheet (nml [Action.Check]) defaultFlop
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns limp coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.Check; Action.Call; Action.RaiseToAmount 100]) defaultFlop
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for single items`` () =
  let actual = pickOopSheet (nml [Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns skips SitBack`` () =
  let actual = pickOopSheet (nml [Action.SitBack; Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.Call; Action.Call]) defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for single item`` () =
  let actual = pickOopSheet (nml [Action.RaiseToAmount 60]) defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for multiple items`` () =
  let actual = pickOopSheet (nml [Action.RaiseToAmount 60; Action.RaiseToAmount 100]) defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", actual)

[<Fact>]
let ``pickOopSheet returns raise FB coorectly for single item`` () =
  let actual = pickOopSheet [(Action.RaiseToAmount 60, Some Bluff)] defaultFlop
  Assert.Equal(Some "hero raise FB vs limp", actual)

[<Fact>]
let ``pickOopSheet falls back to limp for empty history and small pot`` () =
  let s = {defaultFlop with VillainBet = 30; Pot = 70}
  let actual = pickOopSheet [] s
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet falls back to call for empty history and bigger pot`` () =
  let s = {defaultFlop with VillainBet = 30; Pot = 110}
  let actual = pickOopSheet [] s
  Assert.Equal(Some "hero call raise pre", actual)

let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
let adxl = openExcel fileNameAdvancedOOP
let bluffy = 
  (importFlopList "bluffy hero ch-r flop vs limp" (fst adxl),
    importFlopList "bluffy hero ch-r flop vs minr" (fst adxl),
    importFlopList "bluffy overtaking, vill ch b fl" (fst adxl))
closeExcel adxl

let testPostFlopMotivated h s mono expected =
  let v = handValueWithDraws s.Hand s.Board
  let t = { Streety = false; DoublePaired = false; Monoboard = mono }

  let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
  let xl = openExcel fileName
  let actual = decidePostFlopOop h s v t xl bluffy
  Assert.Equal(expected, actual.Value.Action)
  closeExcel xl

let testPostFlop h s mono expected =
  let mh = h |> List.map notMotivated
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
let ``decidePostFlop ovso special rule on turn`` () =
  let s = { Hand = parseSuitedHand "7s8s"; Board = parseBoard "5s7c3hTd"; Pot = 200; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 40; Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 135)

[<Fact>]
let ``decidePostFlop A/f special rule on turn`` () =
  let s = { Hand = parseSuitedHand "7sTs"; Board = parseBoard "8cKsJdAc"; Pot = 200; VillainStack = 300; HeroStack = 600; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 40; Action.RaiseToAmount 60] s 0 (Action.RaiseToAmount 135)

[<Fact>]
let ``decidePostFlop 61 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "Td7s"; Board = parseBoard "5hTh9sAs"; Pot = 220; VillainStack = 380; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 30 }
  testPostFlop [Action.Check; Action.Check; Action.RaiseToAmount 80] s 0 (Action.RaiseToAmount 220)

[<Fact>]
let ``decidePostFlop 7 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "8d9h"; Board = parseBoard "9cQd8s6h"; Pot = 100; VillainStack = 450; HeroStack = 450; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 75)

[<Fact>]
let ``decidePostFlop 5 special rule on turn`` () =
  let s = { Hand = parseSuitedHand "QsKh"; Board = parseBoard "TdQc9d5h"; Pot = 100; VillainStack = 380; HeroStack = 520; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 (Action.RaiseToAmount 75)

[<Fact>]
let ``decidePostFlop give up turn after bluffy flop check raise`` () =
  let s = { Hand = parseSuitedHand "4hQc"; Board = parseBoard "Ts7c5d5h"; Pot = 300; VillainStack = 350; HeroStack = 350; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check; Action.RaiseToAmount 110] s 0 Action.Check

[<Fact>]
let ``decidePostFlop no check raise on bluffy flop with nothing`` () =
  let s = { Hand = parseSuitedHand "6h9h"; Board = parseBoard "Jd7h3d"; Pot = 120; VillainStack = 410; HeroStack = 450; VillainBet = 40; HeroBet = 0; BB = 20 }
  testPostFlop [Action.Call; Action.Check] s 0 Action.Fold

[<Fact>]
let ``decidePostFlop all in on turn after bluffy check/raise on flop`` () =
  let s = { Hand = parseSuitedHand "6h9h"; Board = parseBoard "Jd7h3d5s"; Pot = 300; VillainStack = 340; HeroStack = 340; VillainBet = 0; HeroBet = 0; BB = 20 }
  let h = [ { Action = Action.Call; Motivation = None }; 
            { Action = Action.Check; Motivation = None }; 
            { Action = Action.RaiseToAmount 110; Motivation = Some Bluff } ]
  testPostFlopMotivated h s 0 Action.AllIn
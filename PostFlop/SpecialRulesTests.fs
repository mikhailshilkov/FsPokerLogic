module SpecialRulesTests

open Xunit
open Cards.Actions
open Hands
open Cards.HandValues
open PostFlop.Decision
open PostFlop.Options
open PostFlop.SpecialRules

let defaultOptions = { First = Check; Then = Fold; Special = []; Scenario = null; SpecialScenario = null }
let defaultFlop = { Hand = parseSuitedHand "Ah5h"; Board = parseBoard "QsTc6s"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultTurn = { Hand = parseSuitedHand "Ah5h"; Board = parseBoard "QsTc6s7d"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultRiver = { Hand = parseSuitedHand "Kh5h"; Board = parseBoard "QsTc6s7dAd"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }

[<Fact>]
let ``specialRulesOop changes CallEQ based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10] }
  let expected = { options with Then = CallEQ 32 }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change CallEQ based on CallEQPlusXvsAI when villain is not AI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop changes CallEQIfRaised based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = CallEQIfRaised(20, 15); Special = [CallEQPlusXvsAI 15] }
  let expected = { options with Then = CallEQIfRaised(35, 30) }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop changes Raise/CallEQ based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = Raise(20m, CallEQ(15)); Special = [CallEQPlusXvsAI 15] }
  let expected = { options with Then = Raise(20m, CallEQ(30)) }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on BoardOvercard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call)] }
  let expected = { options with First = Check; Then = Call }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = King; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on BoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call)] }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = Jack; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns AI based on AllInBoardAce`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn)] }
  let expected = { options with First = OopDonk.AllIn; Then = AllIn }
  let s = { defaultFlop with Board = parseBoard "5h7dJsAc" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on AllInBoardAce when Ace is paired`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn)] }
  let s = { defaultFlop with Board = parseBoard "5h7dAsAc" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on CallEQOnPairedBoard`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22)] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CallEQOnPairedBoard when board is not paired`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "5h7d8s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns Donk based on CheckCheck`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns Stack Off based on CheckCheck`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Turn 0 (Action.RaiseToAmount 75)]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheck when last action is not check`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheck when last action turn check but flop was call`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call
    notMotivated Turn 0 (Action.RaiseToAmount 75)]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does not fail CheckCheck when no last action`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns first based on CheckCheckAndBoardOvercard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let expected = { options with First = Donk 75m; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQs" }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns then based on CheckCheckAndBoardOvercard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let expected = { options with First = Donk 75m; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQs" }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckAndBoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "Js6c9dJc" }
  let expected = { options with Then = StackOff }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Turn 0 (Action.RaiseToAmount 75)]
  let actual = specialRulesOop s history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckAndBoardOvercard when not check-check`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQc" }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call]
  let actual = specialRulesOop s history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on KHighOnPaired on 1st blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd" }
  let expected = { options with Then = CallEQ 30 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns based on KHighOnPaired on 2nd blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd"; BB = 30 }
  let expected = { options with Then = CallEQ 25 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when board is not paired`` () =
  let options = { defaultOptions with Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d8s"; Hand = parseSuitedHand "KsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when Then is not fold`` () =
  let options = { defaultOptions with Then = StackOff; Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when no K-high`` () =
  let options = { defaultOptions with Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "QsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired on higher blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd"; BB = 50 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on CheckRaiseOvercardBluff on overcard and mid-sized bet in deep stack`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10))] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90 }
  let expected = { options with Then = Raise(2.75m, OopOnCBet.CallEQ 10) }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when no overcard`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10))] }
  let s = { defaultTurn with Board = parseBoard "5h7d9s2s"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Theory>]
[<InlineData(55)>]
[<InlineData(110)>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when bet is too small or big`` vb =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10))] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 180 + vb; VillainBet = vb }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when stack is low`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10))] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90; HeroStack = 240 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop applies not-first rule from the list too`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn); BoardOvercard(Check,Fold); PairedBoard (Check, CallEQ 22)] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s2s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop applies many rules in order`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn); BoardOvercard(Check,Fold)] }
  let expected = { options with First = OopDonk.AllIn; Then = AllIn }
  let s = { defaultFlop with Board = parseBoard "5h6d7sAs" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

let never = fun _ -> false

let strategicRulesOop' s h bluffyCheckRaiseFlops =
  let value = handValueWithDraws s.Hand s.Board
  let history = h |> List.map (notMotivated Flop 0)
  strategicRulesOop s value history bluffyCheckRaiseFlops (never, never) defaultOptions

let strategicRulesOop2' s h bluffyCheckRaiseFlops =
  let value = handValueWithDraws s.Hand s.Board
  strategicRulesOop s value h bluffyCheckRaiseFlops (never, never) defaultOptions

let strategicRulesOop3' s h bluffyCheckRaiseFlops hands =
  let value = handValueWithDraws s.Hand s.Board
  let history = h |> List.map (notMotivated Flop 0)
  strategicRulesOop s value history bluffyCheckRaiseFlops hands defaultOptions

[<Fact>]
let ``strategicRulesOop takes over Turn check-check on flop, no Ace and low deep stack`` () =
  let expected = { defaultOptions with First = Donk 75m }
  let actual = strategicRulesOop3' defaultTurn [Action.Check; Action.Check] ([], [], []) (never, never >> not)
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop cbets River after take over Turn`` () =
  let s = { defaultRiver with Pot = 248 }
  let expected = { defaultOptions with First = Donk 50m }
  let actual = strategicRulesOop3' s [Action.Check; Action.Check; Action.RaiseToAmount 75] ([], [], []) (never, never >> not)
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop AI on turn after c/r on flop with small stack in limped pot`` () =
  let s = { defaultTurn with HeroStack = 120 }
  let actual = strategicRulesOop' s [Action.Check; Action.Check; Action.RaiseToAmount 90] ([], [], [])
  let expected = { defaultOptions with First = OopDonk.AllIn }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop check/call paired turn after calling flop with second pair`` () =
  let s = { defaultTurn with Hand = parseSuitedHand "Ks9c"; Board = parseBoard "Qd9h6dQs" }
  let actual = strategicRulesOop' s [Action.RaiseToAmount 60; Action.Call] ([], [], [])
  let expected = { defaultOptions with Then = Call }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop bluffy check raise flop minraised pf`` () =
  let s = { defaultFlop with Hand = parseSuitedHand "7s4c"; Board = parseBoard "3d3h6s"; VillainBet = 40; Pot = 120 }
  let actual = strategicRulesOop' s [Action.Call; Action.Check] ([], [[Three;Three;Six]], [])
  let expected = { defaultOptions with Then = Raise(2.75m, OopOnCBet.Fold) }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop all in turn with Nothing & overcard after bluffy check raise flop minraised pf`` () =
  let s = { defaultTurn with Hand = parseSuitedHand "7s2c"; Board = parseBoard "3d3h6s8d"; Pot = 300 }
  let h = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check; bluff Flop 40 110]
  let actual = strategicRulesOop2' s h ([], [[Three;Three;Six]], [])
  let expected = { defaultOptions with First = OopDonk.AllIn }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop all in turn with Gutshot after bluffy check raise flop minraised pf`` () =
  let s = { defaultTurn with Hand = parseSuitedHand "7s4c"; Board = parseBoard "3d3hTs5d"; Pot = 300 }
  let h = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check; bluff Flop 40 110]
  let actual = strategicRulesOop2' s h ([], [[Three;Three;Ten]], [])
  let expected = { defaultOptions with First = OopDonk.AllIn }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop bluffy check raise flop limped pf`` () =
  let s = { defaultFlop with Hand = parseSuitedHand "Qs5c"; Board = parseBoard "5d8hTs"; VillainBet = 30; Pot = 70 }
  let actual = strategicRulesOop' s [Action.Check; Action.Check] ([[Five;Eight;Ten]], [], [])
  let expected = { defaultOptions with Then = Raise(4m, OopOnCBet.Fold) }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop donk any turn after bluffy check raise flop limped pf`` () =
  let s = { defaultTurn with Hand = parseSuitedHand "Qs5c"; Board = parseBoard "5d8hTs2s"; Pot = 280 }
  let h = [notMotivated PreFlop 40 Action.Check; notMotivated Flop 0 Action.Check; bluff Flop 40 120]
  let actual = strategicRulesOop2' s h ([[Five;Eight;Ten]], [], [])
  let expected = { defaultOptions with First = Donk 62.5m }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop AI overcard river after bluffy check raise flop and donk turn limped pf`` () =
  let s = { defaultRiver with Hand = parseSuitedHand "Qs5c"; Board = parseBoard "5d8hTs2sJd"; Pot = 630 }
  let h = [
    notMotivated PreFlop 40 Action.Check
    notMotivated Flop 0 Action.Check
    bluff Flop 40 120
    bluff Turn 0 175]
  let actual = strategicRulesOop2' s h ([[Five;Eight;Ten]], [], [])
  let expected = { defaultOptions with First = OopDonk.AllIn }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop bluff overtake non-A turn after no cbet after pfr`` () =
  let s = { defaultTurn with Hand = parseSuitedHand "9s8c"; Board = parseBoard "Qs2dThKs"; Pot = 100 }
  let h = [Action.Call; Action.Check]
  let actual = strategicRulesOop' s h ([], [], [[Two;Ten;Queen]])
  let expected = { defaultOptions with First = Donk 75m }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop bet non-A river after bluff overtake after no cbet after pfr`` () =
  let s = { defaultRiver with Hand = parseSuitedHand "5s5c"; Board = parseBoard "Qs2dThKs4c"; Pot = 250 }
  let h = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check; bluff Turn 0 75]
  let actual = strategicRulesOop2' s h ([], [], [[Two;Ten;Queen]])
  let expected = { defaultOptions with First = Donk 62.5m }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``scenarioRulesOop applies scenario if last bet was same scenario`` () =
  let h = [notMotivated PreFlop 40 Action.Call; bluff Flop 0 75; scenario Turn 0 (Action.RaiseToAmount 175) "r8"]
  let options = { defaultOptions with Scenario = "r8/20" }
  let actual = scenarioRulesOop defaultRiver h options
  let expected = { options with First = RiverBetSizing; Then = CallEQ 20 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``scenarioRulesOop applies scenario if last bet was substring of current scenario`` () =
  let h = [notMotivated PreFlop 40 Action.Call; bluff Flop 0 75; scenario Turn 0 (Action.RaiseToAmount 175) "r9"]
  let options = { defaultOptions with Scenario = "r8r9/8" }
  let actual = scenarioRulesOop defaultRiver h options
  let expected = { options with First = RiverBetSizing; Then = CallEQ 8 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``scenarioRulesOop does not apply if last bet was not same scenario`` () =
  let h = [notMotivated PreFlop 40 Action.Call; bluff Flop 0 75; scenario Turn 0 (Action.RaiseToAmount 175) "r9"]
  let options = { defaultOptions with Scenario = "r8/20" }
  let actual = scenarioRulesOop defaultRiver h options
  Assert.Equal(options, actual)

[<Fact>]
let ``scenarioRulesOop does not apply if current scenario has no call eq`` () =
  let h = [notMotivated PreFlop 40 Action.Call; bluff Flop 0 75; scenario Turn 0 (Action.RaiseToAmount 175) "r8"]
  let options = { defaultOptions with Scenario = "r8" }
  let actual = scenarioRulesOop defaultRiver h options
  Assert.Equal(options, actual)
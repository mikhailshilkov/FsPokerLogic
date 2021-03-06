﻿module SpecialRulesTests

open Xunit
open Cards.Actions
open Hands
open Cards.HandValues
open PostFlop.Decision
open PostFlop.Options
open PostFlop.SpecialRules

let defaultOptions = { First = Check; Then = Fold; Special = []; Scenario = null }
let defaultFlop = { Hand = parseSuitedHand "Ah5h"; Board = parseBoard "QsTc6s"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultTurn = { Hand = parseSuitedHand "Ah5h"; Board = parseBoard "QsTc6s7d"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultRiver = { Hand = parseSuitedHand "Kh5h"; Board = parseBoard "QsTc6s7dAd"; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
let defaultTexture = { Streety = false; DoublePaired = false; ThreeOfKind = false; FourOfKind = false; Monoboard = 2 }

[<Fact>]
let ``specialRulesOop changes CallEQ based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10, null] }
  let expected = { options with Then = CallEQ 32 }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change CallEQ based on CallEQPlusXvsAI when villain is not AI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10, null] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop changes CallEQIfRaised based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = CallEQIfRaised(20, 15); Special = [CallEQPlusXvsAI 15, null] }
  let expected = { options with Then = CallEQIfRaised(35, 30) }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop changes Raise/CallEQ based on CallEQPlusXvsAI`` () =
  let options = { defaultOptions with Then = Raise(20m, CallEQ(15)); Special = [CallEQPlusXvsAI 15, null] }
  let expected = { options with Then = Raise(20m, CallEQ(30)) }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on BoardOvercard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call), null] }
  let expected = { options with First = Check; Then = Call }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = King; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on BoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call), null] }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = Jack; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on BoardOvercardNotAce`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercardNotAce(Check,Call), null] }
  let expected = { options with First = Check; Then = Call }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = King; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on BoardOvercardNotAce when not an overcard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercardNotAce(Check,Call), null] }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = Jack; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on BoardOvercardNotAce when ace`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercardNotAce(Check,Call), null] }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = Ace; Suit = Hearts}|] }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns AI based on AllInBoardAce`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn), null] }
  let expected = { options with First = OopDonk.AllIn; Then = AllIn }
  let s = { defaultFlop with Board = parseBoard "5h7dJsAc" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on AllInBoardAce when Ace is paired`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn), null] }
  let s = { defaultFlop with Board = parseBoard "5h7dAsAc" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on CallEQOnPairedBoard`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22), null] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CallEQOnPairedBoard when board is not paired`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22), null] }
  let s = { defaultFlop with Board = parseBoard "5h7d8s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns Donk based on CheckCheck`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff), null] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns Stack Off based on CheckCheck`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff), null] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Turn 0 (Action.RaiseToAmount 75)]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheck when last action is not check`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff), null] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheck when last action turn check but flop was call`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff), null] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call
    notMotivated Turn 0 (Action.RaiseToAmount 75)]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does not fail CheckCheck when no last action`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff), null] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns first based on CheckCheckAndBoardOvercard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22), null] }
  let expected = { options with First = Donk 75m; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQs" }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns then based on CheckCheckAndBoardOvercard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22), null] }
  let expected = { options with First = Donk 75m; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQs" }
  let history = [notMotivated PreFlop 40 Action.Call; notMotivated Flop 0 Action.Check]
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckAndBoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22), null] }
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
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22), null] }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQc" }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call]
  let actual = specialRulesOop s history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns Stack Off based on CheckCheckChechCheck`` () =
  let options = { defaultOptions with Special = [CheckCheckCheckCheck (Donk 75m, StackOff), null] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Turn 0 Action.Check]
  let actual = specialRulesOop defaultRiver history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckCheckCheck when turn action is not check`` () =
  let options = { defaultOptions with Special = [CheckCheckCheckCheck (Donk 75m, StackOff), null] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 0 Action.Check
    notMotivated Turn 40 Action.Call]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckCheckCheck when flop action is not check`` () =
  let options = { defaultOptions with Special = [CheckCheckCheckCheck (Donk 75m, StackOff), null] }
  let history = [
    notMotivated PreFlop 40 Action.Call 
    notMotivated Flop 40 Action.Call
    notMotivated Turn 0 Action.Check]
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on KHighOnPaired on 1st blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd" }
  let expected = { options with Then = CallEQ 30 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop returns based on KHighOnPaired on 2nd blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd"; BB = 30 }
  let expected = { options with Then = CallEQ 25 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when board is not paired`` () =
  let options = { defaultOptions with Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d8s"; Hand = parseSuitedHand "KsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when Then is not fold`` () =
  let options = { defaultOptions with Then = StackOff; Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired when no K-high`` () =
  let options = { defaultOptions with Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "QsJd" }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on KHighOnPaired on higher blind level`` () =
  let options = { defaultOptions with Special = [KHighOnPaired, null] }
  let s = { defaultFlop with Board = parseBoard "5h7d7s"; Hand = parseSuitedHand "KsJd"; BB = 50 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on CheckRaiseOvercardBluff on overcard and mid-sized bet in deep stack`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10)), null] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90 }
  let expected = { options with Then = Raise(2.75m, OopOnCBet.CallEQ 10) }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when no overcard`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10)), null] }
  let s = { defaultTurn with Board = parseBoard "5h7d9s2s"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Theory>]
[<InlineData(55)>]
[<InlineData(110)>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when bet is too small or big`` vb =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10)), null] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 180 + vb; VillainBet = vb }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckRaiseOvercardBluff when stack is low`` () =
  let options = { defaultOptions with Special = [CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10)), null] }
  let s = { defaultTurn with Board = parseBoard "5h7d9sQs"; Hand = parseSuitedHand "KsJd"; Pot = 270; VillainBet = 90; HeroStack = 240 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on SlowPlayedBefore if there was a slow play before`` () =
  let options = { defaultOptions with Special = [SlowPlayedBefore(OopOnCBet.CallEQ 30), null] }
  let expected = { options with Then = OopOnCBet.CallEQ 30 }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 40)
    notMotivated Flop 0 (Action.RaiseToAmount 40)
    slowPlay Turn]
  let actual = specialRulesOop defaultRiver history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on SlowPlayedBefore when there was no slowplay`` () =
  let options = { defaultOptions with Special = [SlowPlayedBefore(OopOnCBet.CallEQ 30), null] }
  let actual = specialRulesOop defaultRiver [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on BarrelX3 if there were 3 barrels`` () =
  let options = { defaultOptions with Special = [BarrelX3(OopOnCBet.CallEQ 30), null] }
  let expected = { options with Then = OopOnCBet.CallEQ 30 }
  let history = [
    notMotivated PreFlop 40 Action.Call
    notMotivated Flop 0 Action.Check
    notMotivated Flop 40 Action.Call
    notMotivated Turn 0 Action.Check
    notMotivated Turn 90 Action.Call
    notMotivated River 0 Action.Check]
  let s = { defaultRiver with VillainBet = 200; Pot = 540 }
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on BarrelX3 when there were 2 gaybets`` () =
  let options = { defaultOptions with Special = [BarrelX3(OopOnCBet.CallEQ 30), null] }
  let history = [
    notMotivated PreFlop 40 Action.Call
    notMotivated Flop 0 Action.Check
    notMotivated Flop 20 Action.Call
    notMotivated Turn 0 Action.Check
    notMotivated Turn 30 Action.Call
    notMotivated River 0 Action.Check]
  let s = { defaultRiver with VillainBet = 100; Pot = 280 }
  let actual = specialRulesOop s history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns Call EQ based on VillainRaised when villain raised last street`` () =
  let options = { defaultOptions with Special = [VillainRaised (Check, CallEQ 30), null] }
  let expected = { options with Then = CallEQ 30 }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 40)
    notMotivated Flop 0 (Action.RaiseToAmount 50)
    notMotivated Flop 150 Action.Call]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on VillainRaised when villain just bet`` () =
  let options = { defaultOptions with Special = [VillainRaised(Check, CallEQ 30), null] }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 40)
    notMotivated Flop 0 Action.Check
    notMotivated Flop 50 Action.Call]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns Stack Off based on HeroRaised when hero raised last street`` () =
  let options = { defaultOptions with Special = [HeroRaised (Donk 75m, StackOff), null] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [
    notMotivated PreFlop 40 Action.Call
    notMotivated Flop 50 (Action.RaiseToAmount 150)]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on HeroRaised when hero just bet`` () =
  let options = { defaultOptions with Special = [HeroRaised(Donk 75m, StackOff), null] }
  let history = [
    notMotivated PreFlop 20 (Action.RaiseToAmount 40)
    notMotivated Flop 0 (Action.RaiseToAmount 40)]
  let actual = specialRulesOop defaultTurn history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns AllIn based on StackPotRatioLessThan when SPR is low`` () =
  let options = { defaultOptions with Special = [StackPotRatioLessThan (1.25m, OopDonk.AllIn, OopOnCBet.AllIn), null] }
  let expected = { options with First = OopDonk.AllIn; Then = OopOnCBet.AllIn }
  let s = { defaultTurn with HeroStack = 220; VillainStack = 200; Pot = 160 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on StackPotRatioLessThan when SPR is high`` () =
  let options = { defaultOptions with Special = [StackPotRatioLessThan (1.25m, OopDonk.AllIn, OopOnCBet.AllIn), null] }
  let s = { defaultTurn with HeroStack = 220; VillainStack = 210; Pot = 160 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns AllIn based on SmartyAllIn when we are about to check/call AI`` () =
  let options = { defaultOptions with Then = OopOnCBet.CallEQ 25; Special = [SmartyAllIn, null] }
  let expected = { options with First = OopDonk.AllIn }
  let s = { defaultTurn with HeroStack = 200; VillainStack = 400; Pot = 400 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on SmartyAllIn when we are about to check/fold on AI`` () =
  let options = { defaultOptions with Then = OopOnCBet.CallEQ 25; Special = [SmartyAllIn, null] }
  let s = { defaultTurn with HeroStack = 250; VillainStack = 350; Pot = 400 }
  let actual = specialRulesOop s [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop applies not-first rule from the list too`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn), null; BoardOvercard(Check,Fold), null; PairedBoard (Check, CallEQ 22), null] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s2s" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop applies many rules in order`` () =
  let options = { defaultOptions with Special = [BoardAce(OopDonk.AllIn, AllIn), null; BoardOvercard(Check,Fold), null] }
  let expected = { options with First = OopDonk.AllIn; Then = AllIn }
  let s = { defaultFlop with Board = parseBoard "5h6d7sAs" }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

let never = fun _ -> false

let strategicRulesOop' s h bluffyCheckRaiseFlops =
  let value = handValueWithDraws s.Hand s.Board
  let history = h |> List.map (notMotivated Flop 0)
  strategicRulesOop s value history defaultTexture bluffyCheckRaiseFlops (never, never) defaultOptions

let strategicRulesOop2' s h bluffyCheckRaiseFlops =
  let value = handValueWithDraws s.Hand s.Board
  strategicRulesOop s value h defaultTexture bluffyCheckRaiseFlops (never, never) defaultOptions

let strategicRulesOop3' s h bluffyCheckRaiseFlops hands =
  let value = handValueWithDraws s.Hand s.Board
  let history = h |> List.map (notMotivated Flop 0)
  strategicRulesOop s value history defaultTexture bluffyCheckRaiseFlops hands defaultOptions

let strategicRulesOop4' s h texture =
  let value = handValueWithDraws s.Hand s.Board
  let history = h |> List.map (notMotivated Flop 0)
  strategicRulesOop s value history texture ([], [], []) (never, never) defaultOptions

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
let ``strategicRulesOop River All In if nut str8 on board`` () =
  let s = { Hand = parseSuitedHand "Qc4h"; Board = parseBoard "AcKhQdJsTh"; Pot = 100; VillainStack = 390; HeroStack = 610; VillainBet = 0; HeroBet = 0; BB = 20 }
  let actual = strategicRulesOop' s [] ([], [], [])
  let expected = { defaultOptions with First = OopDonk.AllIn; Then = AllIn }
  Assert.Equal(expected, fst actual)

[<Fact>]
let ``strategicRulesOop River Call if nut str8 on 3-spade board`` () =
  let s = { Hand = parseSuitedHand "Qc4h"; Board = parseBoard "AhKhQdJsTh"; Pot = 140; VillainStack = 390; HeroStack = 610; VillainBet = 40; HeroBet = 0; BB = 20 }
  let texture = { defaultTexture with Monoboard = 3 }
  let actual = strategicRulesOop4' s [] texture
  let expected = { defaultOptions with First = OopDonk.Check; Then = CallEQ 32 }
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
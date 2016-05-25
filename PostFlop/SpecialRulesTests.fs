module SpecialRulesTests

open Xunit
open Cards
open Hands
open PostFlop.Decision
open PostFlop.Options
open PostFlop.SpecialRules

let defaultOptions = { First = Check; Then = Fold; Special = [] }
let defaultFlop = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}|]; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }

[<Fact>]
let ``specialRulesOop changes CallEQ based on CallEQPlus10vsAI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10] }
  let expected = { options with Then = CallEQ 32 }
  let s = { defaultFlop with VillainStack = 0 }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change CallEQ based on CallEQPlus10vsAI when villain is not AI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

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
let ``specialRulesOop returns based on CheckCheck`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let expected = { options with First = Donk 75m; Then = StackOff }
  let history = [Action.Call; Action.Check] // preflop call - flop check check
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheck when last action is not check`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let history = [Action.Call; Action.Check; Action.Call] // preflop call - flop check call
  let actual = specialRulesOop defaultFlop history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does not fail CheckCheck when no last action`` () =
  let options = { defaultOptions with Special = [CheckCheck (Donk 75m, StackOff)] }
  let actual = specialRulesOop defaultFlop [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns based on CheckCheckAndBoardOvercard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let expected = { options with First = Donk 75m; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQs" }
  let history = [Action.Call; Action.Check] // preflop call - flop check check
  let actual = specialRulesOop s history options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckAndBoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "Js6c9dJc" }
  let history = [Action.Call; Action.Check] // preflop call - flop check check
  let actual = specialRulesOop s history options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCheckAndBoardOvercard when not check-check`` () =
  let options = { defaultOptions with Special = [CheckCheckAndBoardOvercard(Donk 75m,CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "Js6c9dQc" }
  let history = [Action.Call; Action.Check; Action.Call] // preflop call - flop check call
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
let ``specialRulesOop returns based on CheckRaiseBluffOnFlop with all conditions true`` () =
  let options = { defaultOptions with Special = [CheckRaiseBluffOnFlop] }
  let s = { defaultFlop with Pot = 120; VillainBet = 40; Board = parseBoard "6s4cJd"; Hand = parseSuitedHand "Ks2d" }
  let expected = { options with First = Check; Then = RaiseFold }
  let actual = specialRulesOop s [] options
  Assert.Equal(expected, actual)

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
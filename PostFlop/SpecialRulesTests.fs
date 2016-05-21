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
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change CallEQ based on CallEQPlus10vsAI when villain is not AI`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [CallEQPlusXvsAI 10] }
  let actual = specialRulesOop defaultFlop options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on CheckCallBoardOvercard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call)] }
  let expected = { options with First = Check; Then = Call }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = King; Suit = Hearts}|] }
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CheckCallBoardOvercard when not an overcard`` () =
  let options = { defaultOptions with Then = CallEQ 22; Special = [BoardOvercard(Check,Call)] }
  let s = { defaultFlop with Board = Array.append defaultFlop.Board [|{Face = Jack; Suit = Hearts}|] }
  let actual = specialRulesOop s options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns AI based on AllInBoardAce`` () =
  let options = { defaultOptions with Special = [BoardAce OopDonk.AllIn] }
  let expected = { options with First = OopDonk.AllIn; Then = Fold }
  let s = { defaultFlop with Board = parseBoard "5h7dJsAc" }
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on AllInBoardAce when Ace is paired`` () =
  let options = { defaultOptions with Special = [BoardAce OopDonk.AllIn] }
  let s = { defaultFlop with Board = parseBoard "5h7dAsAc" }
  let actual = specialRulesOop s options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop returns check/call based on CallEQOnPairedBoard`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22)] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s" }
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop does no change based on CallEQOnPairedBoard when board is not paired`` () =
  let options = { defaultOptions with Special = [PairedBoard (Check, CallEQ 22)] }
  let s = { defaultFlop with Board = parseBoard "5h7d8s" }
  let actual = specialRulesOop s options
  Assert.Equal(options, actual)

[<Fact>]
let ``specialRulesOop applies not-first rule from the list too`` () =
  let options = { defaultOptions with Special = [BoardAce OopDonk.AllIn; BoardOvercard(Check,Fold); PairedBoard (Check, CallEQ 22)] }
  let expected = { options with First = Check; Then = CallEQ 22 }
  let s = { defaultFlop with Board = parseBoard "5h7d7s2s" }
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)

[<Fact>]
let ``specialRulesOop applies many rules in order`` () =
  let options = { defaultOptions with Special = [BoardAce OopDonk.AllIn; BoardOvercard(Check,Fold)] }
  let expected = { options with First = OopDonk.AllIn }
  let s = { defaultFlop with Board = parseBoard "5h6d7sAs" }
  let actual = specialRulesOop s options
  Assert.Equal(expected, actual)
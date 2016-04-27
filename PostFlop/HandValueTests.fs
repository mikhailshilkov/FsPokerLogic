module HandValueTests

open Xunit
open PostFlop.Options
open PostFlop.Decision
open PostFlop.HandValue
open Hands

let defaultFlop = { Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20; Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [||] }
let defaultTurn = { Pot = 180; VillainStack = 440; HeroStack = 380; VillainBet = 0; HeroBet = 0; BB = 20; Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [||] }

let defaultOptions = { 
  CbetFactor = Never
  CheckRaise = OnCheckRaise.Call
  Donk = OnDonk.Undefined }

[<Fact>]
let ``augmentOptions returns input option when no second pair`` () =
  let hand = parseSuitedHand "7c5d"
  let board = parseBoard "Ac8d2h"
  let snapshot = { defaultFlop with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when second pair but not donk all-in`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d2h"
  let snapshot = { defaultFlop with VillainStack = 10; VillainBet = 200; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when second pair with K+ kicker on turn`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d3s6d"
  let snapshot = { defaultTurn with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns donk all-in call when second pair with K+ kicker`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d3s"
  let snapshot = { defaultFlop with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  let expected = { defaultOptions with Donk = OnDonk.CallRaisePet }
  Assert.Equal(expected, actual)

[<Fact>]
let ``augmentOptions returns input option when no-call on 3-suited turn`` () =
  let hand = parseSuitedHand "Jc8s"
  let board = parseBoard "Ks9c3c5c"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let options = { defaultOptions with CheckRaise = OnCheckRaise.Fold }
  let actual = augmentOptions snapshot options
  Assert.Equal(options, actual)

[<Fact>]
let ``augmentOptions returns input option when Q+ FD on 3-suited turn`` () =
  let hand = parseSuitedHand "Kc8s"
  let board = parseBoard "Ks9c3c5c"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when no Q+ FD on 2-suited turn`` () =
  let hand = parseSuitedHand "Jc8c"
  let board = parseBoard "Ks9c3c5s"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns check raise stack off when no Q+ FD on 3-suited turn`` () =
  let hand = parseSuitedHand "Kc8s"
  let board = parseBoard "Ks9d3s5s"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultOptions
  let expected = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("Ks8d", "Ts8s2h", true)>]
[<InlineData("AsQd", "3hKsQs", true)>]
[<InlineData("Ks3d", "3hQcQs", true)>]
[<InlineData("KsJd", "3hKsQs", false)>]
[<InlineData("JsJd", "3hKsQs", false)>]
[<InlineData("KsQd", "3hQcQs", false)>]
[<InlineData("KsKd", "3hKcQs", false)>]
let ``isSecondPairWithAKKicker works`` handString boardString expected = 
  let hand = parseSuitedHand handString
  let board = parseBoard boardString
  let actual = isSecondPairWithAKKicker hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("Ks8d", "Ts8s2h8s", true)>]
[<InlineData("Qs8d", "Ts8s2h8s", true)>]
[<InlineData("As8d", "Ts8s2h8s", true)>]
[<InlineData("Ks8d", "Ts8s2s8s", false)>]
[<InlineData("Kd8s", "Ts9s2h8s", false)>]
[<InlineData("Kd8h", "Ts8s2s8s", false)>]
[<InlineData("Ks8s", "Ts8s2h8d", false)>]
let ``isFlushDrawWithAKQKicker works`` handString boardString expected = 
  let hand = parseSuitedHand handString
  let board = parseBoard boardString
  let actual = isSingleCardFlushDrawWithAKQKicker hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("Ts8s2h6s", true)>]
[<InlineData("3hKcQs6s", false)>]
[<InlineData("3hKcQs6h", false)>]
[<InlineData("3hKhQh6h", false)>]
let ``is3SuitedCardsOnBoard works`` boardString expected = 
  let board = parseBoard boardString
  let actual = is3SuitedCardsOnBoard board
  Assert.Equal(expected, actual)

module HandValueTests

open Xunit
open Cards.Actions
open PostFlop.Options
open PostFlop.Decision
open PostFlop.HandValue
open Hands
open Cards.HandValues

let noDraw value = { Made = value; SD = NoSD; FD = NoFD; FD2 = NoFD }

let defaultFlop = { Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20; Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [||] }
let defaultTurn = { Pot = 180; VillainStack = 440; HeroStack = 380; VillainBet = 0; HeroBet = 0; BB = 20; Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [||] }
let defaultRiver = { Pot = 380; VillainStack = 340; HeroStack = 280; VillainBet = 0; HeroBet = 0; BB = 20; Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [||] }
let defaultValue = Nothing |> noDraw
let defaultTexture = { Streety = false; DoublePaired = false; ThreeOfKind = false; FourOfKind = false; Monoboard = 2 }

let defaultOptions = { 
  CbetFactor = Never
  CheckRaise = OnCheckRaise.Call
  Donk = OnDonk.Undefined
  DonkRaise = OnDonkRaise.Undefined }

[<Fact>]
let ``augmentOptions returns input option when no second pair`` () =
  let hand = parseSuitedHand "7c5d"
  let board = parseBoard "Ac8d2h"
  let snapshot = { defaultFlop with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let value = Pair(Third) |> noDraw
  let actual = augmentOptions snapshot value defaultTexture [] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when second pair but not donk all-in`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d2h"
  let snapshot = { defaultFlop with VillainStack = 10; VillainBet = 200; Hand = hand; Board = board }
  let value = Pair(Second(Ace)) |> noDraw
  let actual = augmentOptions snapshot value defaultTexture [] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when second pair with K+ kicker on turn`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d3s6d"
  let snapshot = { defaultTurn with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let value = Pair(Second(Ace)) |> noDraw
  let actual = augmentOptions snapshot value defaultTexture [] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns donk all-in call when second pair with K+ kicker`` () =
  let hand = parseSuitedHand "Ks8s"
  let board = parseBoard "Ac8d3s"
  let snapshot = { defaultFlop with VillainStack = 0; VillainBet = 490; Hand = hand; Board = board }
  let value = Pair(Second(Ace)) |> noDraw
  let actual = augmentOptions snapshot value defaultTexture [] defaultOptions
  let expected = { defaultOptions with Donk = OnDonk.CallRaisePet }
  Assert.Equal(expected, actual)

[<Fact>]
let ``augmentOptions returns input option when no-call on 3-suited turn`` () =
  let hand = parseSuitedHand "Jc8s"
  let board = parseBoard "Ks9c3c5c"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let options = { defaultOptions with CheckRaise = OnCheckRaise.Fold }
  let actual = augmentOptions snapshot defaultValue defaultTexture [] options
  Assert.Equal(options, actual)

[<Fact>]
let ``augmentOptions returns input option when Q+ FD on 3-suited turn`` () =
  let hand = parseSuitedHand "Kc8s"
  let board = parseBoard "Ks9c3c5c"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultValue defaultTexture [] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns input option when no Q+ FD on 2-suited turn`` () =
  let hand = parseSuitedHand "Jc8c"
  let board = parseBoard "Ks9c3c5s"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultValue defaultTexture [] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``augmentOptions returns check raise stack off when no Q+ FD on 3-suited turn`` () =
  let hand = parseSuitedHand "Kc8s"
  let board = parseBoard "Ks9d3s5s"
  let snapshot = { defaultTurn with HeroBet = 50; VillainBet = 150; Hand = hand; Board = board }
  let actual = augmentOptions snapshot defaultValue defaultTexture [] defaultOptions
  let expected = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("Ks8s", "Qs9d3s5c")>]
[<InlineData("2s8s", "QsAd3s5c")>]
[<InlineData("2s8s", "5s9d3sQc")>]
let ``cbetFlushDrawOnTurn Cbet FD card with one overcard or ace on board or overcard on board`` handString boardString =
  let snapshot = { defaultTurn with Board = parseBoard boardString; Hand = parseSuitedHand handString }
  let actual = cbetFlushDrawOnTurn snapshot defaultValue defaultTexture defaultOptions
  let expected = { defaultOptions with Options.CbetFactor = OrAllIn { DefaultCBetOr with Factor = 75m; IfRemainingChipsLessThan = 99 }; CheckRaise = OnCheckRaise.Call }
  Assert.Equal(expected, actual)

[<Fact>]
let ``cbetFlushDrawOnTurn no Cbet without overcard`` () =
  let hand = parseSuitedHand "Js8s"
  let board = parseBoard "Qs9d3s5c"
  let snapshot = { defaultTurn with Board = board; Hand = hand }
  let actual = cbetFlushDrawOnTurn snapshot defaultValue defaultTexture defaultOptions
  let expected = { defaultOptions with Options.CbetFactor = Never }
  Assert.Equal(expected, actual)

[<Fact>]
let ``cbetFlushDrawOnTurn stack off after Cbet FD with decent hand value`` () =
  let hand = parseSuitedHand "9s8s"
  let board = parseBoard "Qs9d3s5c"
  let snapshot = { defaultTurn with Board = board; Hand = hand }
  let value = { defaultValue with Made = Pair(Second(Nine)) }
  let actual = cbetFlushDrawOnTurn snapshot value defaultTexture defaultOptions
  let expected = { defaultOptions with Options.CbetFactor = OrAllIn { DefaultCBetOr with Factor = 75m; IfRemainingChipsLessThan = 99 }; CheckRaise = OnCheckRaise.StackOff }
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("Ts8s", "2s9d3s5cJd")>]
[<InlineData("2s8s", "5s9d3sQc4d")>]
[<InlineData("2s8s", "QsAd3s5c4d")>]
let ``bluffMissedFlushDrawOnRiver bluff missed FD with turn/river overcard or ace on flop`` handString boardString =
  let snapshot = { defaultRiver with Board = parseBoard boardString; Hand = parseSuitedHand handString }
  let actual = bluffMissedFlushDrawOnRiver snapshot Nothing [Action.RaiseToAmount 40; Action.RaiseToAmount 50; Action.RaiseToAmount 100] defaultOptions
  let expected = { defaultOptions with Options.CbetFactor = OrAllIn { DefaultCBetOr with Factor = 70m; IfRemainingChipsLessThan = 79 }; CheckRaise = OnCheckRaise.CallEQ 5 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``bluffMissedFlushDrawOnRiver no bluff when no overcards`` () =
  let snapshot = { defaultRiver with Board = parseBoard "Js9d3s5c2d"; Hand = parseSuitedHand "Ts8s" }
  let actual = bluffMissedFlushDrawOnRiver snapshot Nothing [Action.RaiseToAmount 40; Action.RaiseToAmount 50; Action.RaiseToAmount 100] defaultOptions
  Assert.Equal(defaultOptions, actual)

[<Fact>]
let ``bluffMissedFlushDrawOnRiver no bluff when no bet on turn`` () =
  let snapshot = { defaultRiver with Board = parseBoard "2s9d3s5cJd"; Hand = parseSuitedHand "Ts8s" }
  let actual = bluffMissedFlushDrawOnRiver snapshot Nothing [Action.RaiseToAmount 40; Action.RaiseToAmount 50; Action.Check] defaultOptions
  Assert.Equal(defaultOptions, actual)

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

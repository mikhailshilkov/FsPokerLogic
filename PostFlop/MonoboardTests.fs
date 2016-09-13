module MonoboardTests

open Xunit
open PostFlop.Options
open PostFlop.Decision
open PostFlop.HandValue
open PostFlop.Facade
open Hands
open Cards.HandValues
open Cards.Actions

let defaultRiver = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}; {Face = Two; Suit = Clubs}; {Face = King; Suit = Clubs}|]; Pot = 380; VillainStack = 340; HeroStack = 280; VillainBet = 0; HeroBet = 0; BB = 20 }

let test5MonoboardRiver s combo expected =
  let v = { Made = Flush(combo); FD = NoFD; FD2 = NoFD; SD = NoSD }
  let t = { Streety = false; DoublePaired = false; ThreeOfKind = false; FourOfKind = false; Monoboard = 5 }
  let actual = decidePostFlopNormal [] s v t null null
  Assert.Equal(expected |> Some, actual)

[<Fact>]
let ``Bet 75% with nut flush on 5-card monoriver`` () =
  let s = { defaultRiver with Pot = 200 }
  test5MonoboardRiver s Nut (Action.RaiseToAmount 150)

[<Fact>]
let ``Raise 2.8x with nut flush on 5-card monoriver`` () =
  let s = { defaultRiver with Pot = 200; VillainBet = 61 }
  test5MonoboardRiver s Nut (Action.RaiseToAmount 170)

[<Fact>]
let ``Bet 50% with high flush on 5-card monoriver`` () =
  let s = { defaultRiver with Pot = 200 }
  test5MonoboardRiver s (NotNut(Jack)) (Action.RaiseToAmount 100)

[<Theory>]
[<InlineData(35, true)>]
[<InlineData(37, false)>]
let ``Call EQ 18 with high flush on 5-card monoriver`` bet call =
  let s = { defaultRiver with Pot = 200 - bet; HeroBet = 20; VillainBet = 20 + bet }
  if call then Action.Call else Action.Fold
  |> test5MonoboardRiver s (NotNut(Queen))

[<Fact>]
let ``Bet 37.5% with low flush on 5-card monoriver`` () =
  let s = { defaultRiver with Pot = 200 }
  test5MonoboardRiver s (NotNut(Ten)) (Action.RaiseToAmount 75)

[<Theory>]
[<InlineData(23, true)>]
[<InlineData(25, false)>]
let ``Call EQ 12 on board flush monoriver`` bet call =
  let s = { defaultRiver with Pot = 200 - bet; VillainBet = bet }
  if call then Action.Call else Action.Fold
  |> test5MonoboardRiver s Board
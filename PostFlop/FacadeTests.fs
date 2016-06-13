module FacadeTests

open Excel.Import
open Xunit
open Cards
open Hands
open Cards.HandValues
open PostFlop.Options
open PostFlop.Decision
open PostFlop.HandValue
open PostFlop.Facade

let defaultFlop = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}|]; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }

[<Fact>]
let ``pickOopSheet returns limp coorectly for single item`` () =
  let actual = pickOopSheet [Action.Check] defaultFlop
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns limp coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.Check; Action.Call; Action.RaiseToAmount 100] defaultFlop
  Assert.Equal(Some "limp and check", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for single items`` () =
  let actual = pickOopSheet [Action.Call] defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns skips SitBack`` () =
  let actual = pickOopSheet [Action.SitBack; Action.Call] defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns call coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.Call; Action.Call] defaultFlop
  Assert.Equal(Some "hero call raise pre", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for single item`` () =
  let actual = pickOopSheet [Action.RaiseToAmount 60] defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", actual)

[<Fact>]
let ``pickOopSheet returns raise FV coorectly for multiple items`` () =
  let actual = pickOopSheet [Action.RaiseToAmount 60; Action.RaiseToAmount 100] defaultFlop
  Assert.Equal(Some "hero raise FV vs limp", actual)

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

let testPostFlop h s made expected =
  let v = { Made = made; FD = NoFD; FD2 = NoFD; SD = NoSD }
  let t = { Streety = false; DoublePaired = false; Monoboard = 3 }

  let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
  let xl = openExcel fileName
  let actual = decidePostFlopOop h s v t xl
  Assert.Equal(expected |> Some, actual)
  closeExcel xl

[<Fact>]
let ``decidePostFlop bet on turn`` () =
  let s = { Hand = parseSuitedHand "TcQh"; Board = parseBoard "TsTd7sQs"; Pot = 240; VillainStack = 400; HeroStack = 400; VillainBet = 0; HeroBet = 0; BB = 20 }
  testPostFlop [Action.RaiseToAmount 60; Action.RaiseToAmount 60] s (FullHouse(Normal)) (Action.RaiseToAmount 150)
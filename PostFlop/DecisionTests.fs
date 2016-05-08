namespace PostFlop

module DecisionTests =

  open Hands
  open Cards
  open Options
  open Decision
  open Xunit

  let defaultOptions = { CbetFactor = Never; CheckRaise = OnCheckRaise.Call; Donk = ForValueStackOff  }
  let defaultFlop = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}|]; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
  let defaultTurn = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}; {Face = Two; Suit = Clubs}|]; Pot = 180; VillainStack = 440; HeroStack = 380; VillainBet = 0; HeroBet = 0; BB = 20 }
  let defaultRiver = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}; {Face = Two; Suit = Clubs}; {Face = King; Suit = Clubs}|]; Pot = 380; VillainStack = 340; HeroStack = 280; VillainBet = 0; HeroBet = 0; BB = 20 }

  [<Theory>]
  [<InlineData(80, 50, 40)>]
  [<InlineData(120, 25, 30)>]
  let ``Condition 1: CBet IP on flop if size is defined`` pot f cbet =
    let options = { defaultOptions with CbetFactor = Always f }
    let snapshot = { defaultFlop with Pot = pot }
    let actual = Decision.decide snapshot options
    Assert.Equal(RaiseToAmount cbet |> Some, actual)

  [<Fact>]
  let ``If CBet is less or equal 1 BB then MinRaise`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultFlop with Pot = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some MinRaise, actual)

  [<Fact>]
  let ``Condition 2: No CBet IP on flop if size is undefined`` () =
    let options = { defaultOptions with CbetFactor = Never }
    let actual = Decision.decide defaultFlop options
    Assert.Equal(Some Check, actual)

  [<Theory>]
  [<InlineData(100, 225)>]
  [<InlineData(90, 200)>]
  let ``Condition 3: Stack off on check-raise`` raise reraise =
    let options = { defaultOptions with CheckRaise = StackOff }
    let snapshot = { defaultFlop with VillainBet = raise; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(RaiseToAmount reraise |> Some, actual)

  [<Fact>]
  let ``Stack off on check-raise - allin if raise is more than 53% of stack`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
    let snapshot = { defaultFlop with VillainBet = 115; HeroBet = 40; HeroStack = 390 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Condition 4: Call on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Call }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Fold on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Fold }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``Condition 4a: All-in instead of call on check-raise and small stack`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Call }
    let snapshot = { defaultFlop with Pot = 460; VillainBet = 320; VillainStack = 140; HeroBet = 60; HeroStack = 400 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``All-in on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.AllIn }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 200; VillainBet = 80; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Call check-raise based on equity 2`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 25 }
    let snapshot = { defaultFlop with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Don't call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 25 }
    let snapshot = { defaultFlop with Pot = 246; VillainBet = 126; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``Fold check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``FV & stack off flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 460; Pot = 140; VillainStack = 400; VillainBet = 75 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 160 |> Some, actual)

  [<Fact>]
  let ``FV & stack off 1 BB flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 100; VillainStack = 470; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 80 |> Some, actual)

  [<Fact>]
  let ``FV & stack off flop donk RaiseToAmount not smaller than minimal`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 140; VillainStack = 410; VillainBet = 80 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.RaiseToAmount 180 |> Some)

  [<Fact>]
  let ``FV & stack off flop donk to all-in`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 310; VillainStack = 240; VillainBet = 250 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call/raise + Pet: 1 BB flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.RaiseToAmount 80 |> Some)

  [<Theory>]
  [<InlineData(30, 90)>]
  [<InlineData(34, 100)>]
  let ``Call/raise + Pet: flop donk RaiseToAmount b/w 2 BB and 50% preflop pot`` bet raise =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 80 + bet; VillainBet = bet }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.RaiseToAmount raise |> Some)

  [<Fact>]
  let ``Call/raise + Pet: flop donk RaiseToAmount more than 50% preflop pot`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 120; VillainBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Call when donk is call`` () =
    let options = { defaultOptions with Donk = Call }
    let snapshot = { defaultFlop with Pot = 120; VillainBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Call/raise + Pet: allin donk is > 50% preflop stack`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with HeroStack = 540; VillainStack = 130; VillainBet = 210 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call/raise + Pet: call allin donk`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with HeroStack = 460; VillainStack = 0; VillainBet = 460; Pot = 540 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)
  
  [<Fact>]
  let ``Call donk based on equity`` () =
    let options = { defaultOptions with Donk = CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Call all-in donk based on equity with EQ >= 26`` () =
    let options = { defaultOptions with Donk = CallEQ 26 }
    let snapshot = { defaultFlop with Pot = 290; VillainStack = 0; VillainBet = 200 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Fold donk based on equity`` () =
    let options = { defaultOptions with Donk = CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 110; VillainBet = 30 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Some Action.Fold)

  [<Fact>]
  let ``Special condition 5: check raised with FD`` () =
    let options = { defaultOptions with CheckRaise = StackOff }
    let snapshot = { defaultFlop with Pot = 237; VillainBet = 117; HeroBet = 40; VillainStack = 293 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet normal for value with effective stack 15bb or more`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultTurn with HeroStack = 210; VillainStack = 610 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 90 |> Some, actual)

  [<Fact>]
  let ``Turn: Cbet all-in for value with effective stack 14bb or less`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { Factor = 50m; IfPreStackLessThan = 14; IfStackFactorLessThan = 100m } }
    let snapshot = { defaultTurn with HeroStack = 200; VillainStack = 620 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet all-in for value with effective stack < 2 bets`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { Factor = 75m; IfStackFactorLessThan = 2m; IfPreStackLessThan = 100 } }
    let snapshot = { defaultTurn with Pot = 160; HeroStack = 230; VillainStack = 610 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet for bluff with effective stack 19bb or more`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultTurn with HeroStack = 288; VillainStack = 532 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 90 |> Some, actual)

  [<Fact>]
  let ``Turn: Do not cbet for bluff with effective stack 18bb or less`` () =
    let options = { defaultOptions with CbetFactor = OrCheck { Factor = 62.5m; IfPreStackLessThan = 18; IfStackFactorLessThan = 100m } }
    let snapshot = { defaultTurn with HeroStack = 273; VillainStack = 547 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Check, actual)

  [<Fact>]
  let ``Turn: Do not cbet for bluff with effective stack < 2.8 bets`` () =
    let options = { defaultOptions with CbetFactor = OrCheck { Factor = 62.5m; IfStackFactorLessThan = 2.8m; IfPreStackLessThan = 0 } }
    let snapshot = { defaultTurn with Pot = 160; HeroStack = 570; VillainStack = 270 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Check, actual)

  [<Fact>]
  let ``Makes no decision on undefined donk`` () =
    let snapshot = { defaultTurn with VillainBet = 100 }
    let options = { defaultOptions with Donk = OnDonk.Undefined }
    let actual = Decision.decide snapshot options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Makes no decision on undefined cbet`` () =
    let options = { defaultOptions with CbetFactor = CBet.Undefined }
    let actual = Decision.decide defaultFlop options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Makes no decision on undefined check-raise`` () =
    let snapshot = { defaultTurn with VillainBet = 100; HeroBet = 40 }
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Undefined }
    let actual = Decision.decide snapshot options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Stack off all-in on river`` () =
    let snapshot = { defaultRiver with Pot = 260; VillainBet = 100; HeroStack = 320; VillainStack = 420 }
    let options = { defaultOptions with Donk = OnDonk.ForValueStackOffX 250 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Stack off 2.5x on river`` () =
    let snapshot = { defaultRiver with Pot = 240; VillainBet = 80; HeroStack = 320; VillainStack = 440 }
    let options = { defaultOptions with Donk = OnDonk.ForValueStackOffX 250 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 200 |> Some, actual)

  [<Fact>]
  let ``Call big donk on river`` () =
    let snapshot = { defaultRiver with Pot = 240; VillainBet = 80; HeroStack = 320; VillainStack = 440 }
    let options = { defaultOptions with Donk = OnDonk.CallRaisePet }
    let actual = Decision.decide snapshot options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Raise small donk on river`` () =
    let snapshot = { defaultRiver with Pot = 230; VillainBet = 70; HeroStack = 320; VillainStack = 450 }
    let options = { defaultOptions with Donk = OnDonk.CallRaisePet }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.RaiseToAmount 175 |> Some, actual)

  [<Fact>]
  let ``Call small check-raise on river`` () =
    let snapshot = { defaultRiver with Pot = 820; VillainBet = 430; HeroBet = 160; HeroStack = 90; VillainStack = 0 }
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 11 }
    let actual = Decision.decide snapshot options
    Assert.Equal(Action.Call |> Some, actual)
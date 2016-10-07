namespace PostFlop

module DecisionTests =

  open Hands
  open Cards.Actions
  open Options
  open Decision
  open Xunit

  let defaultOptions = { CbetFactor = Never; CheckRaise = OnCheckRaise.Call; Donk = OnDonk.Fold; DonkRaise = OnDonkRaise.Undefined  }
  let defaultOopOptions = { First = Check; Then = Fold; Special = []; Scenario = null; SpecialScenario = null }
  let defaultFlop = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}|]; Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }
  let defaultTurn = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}; {Face = Two; Suit = Clubs}|]; Pot = 180; VillainStack = 440; HeroStack = 380; VillainBet = 0; HeroBet = 0; BB = 20 }
  let defaultRiver = { Hand = { Card1 = {Face = Ace; Suit = Hearts}; Card2 = {Face = Five; Suit = Hearts} }; Board = [|{Face = Queen; Suit = Spades}; {Face = Ten; Suit = Clubs}; {Face = Six; Suit = Spades}; {Face = Two; Suit = Clubs}; {Face = King; Suit = Clubs}|]; Pot = 380; VillainStack = 340; HeroStack = 280; VillainBet = 0; HeroBet = 0; BB = 20 }

  [<Theory>]
  [<InlineData(80, 50, 40)>]
  [<InlineData(120, 25, 30)>]
  let ``Condition 1: CBet IP on flop if size is defined`` pot f cbet =
    let options = { defaultOptions with CbetFactor = Always f }
    let snapshot = { defaultFlop with Pot = pot }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(RaiseToAmount cbet |> Some, actual)

  [<Fact>]
  let ``If CBet is less or equal 1 BB then MinRaise`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultFlop with Pot = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(RaiseToAmount 20 |> Some, actual)

  [<Fact>]
  let ``Condition 2: No CBet IP on flop if size is undefined`` () =
    let options = { defaultOptions with CbetFactor = Never }
    let actual = Decision.decide [] defaultFlop [] options
    Assert.Equal(Some Action.Check, actual)

  [<Theory>]
  [<InlineData(100, 225)>]
  [<InlineData(90, 200)>]
  let ``Condition 3: Stack off on check-raise`` raise reraise =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
    let snapshot = { defaultFlop with VillainBet = raise; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(RaiseToAmount reraise |> Some, actual)

  [<Fact>]
  let ``Stack off on check-raise - allin if raise is more than 53% of stack`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
    let snapshot = { defaultFlop with VillainBet = 115; HeroBet = 40; HeroStack = 390 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Condition 4: Call on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Call }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Fold on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Fold }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``Condition 4a: All-in instead of call on check-raise and small stack`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Call }
    let snapshot = { defaultFlop with Pot = 460; VillainBet = 320; VillainStack = 140; HeroBet = 60; HeroStack = 400 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``All-in on check-raise`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.AllIn }
    let snapshot = { defaultFlop with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 200; VillainBet = 80; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Call check-raise based on equity 2`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 25 }
    let snapshot = { defaultFlop with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Don't call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 25 }
    let snapshot = { defaultFlop with Pot = 246; VillainBet = 126; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``Fold check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Fold, actual)

  [<Fact>]
  let ``FV & stack off flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 460; Pot = 140; VillainStack = 400; VillainBet = 75 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 160 |> Some, actual)

  [<Fact>]
  let ``FV & stack off 1 BB flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 100; VillainStack = 470; VillainBet = 20 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 80 |> Some, actual)

  [<Fact>]
  let ``FV & stack off flop donk RaiseToAmount not smaller than minimal`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 140; VillainStack = 410; VillainBet = 80 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.RaiseToAmount 180 |> Some)

  [<Fact>]
  let ``FV & stack off flop donk to all-in`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultFlop with HeroStack = 430; Pot = 310; VillainStack = 240; VillainBet = 250 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call/raise + Pet: 1 BB flop donk RaiseToAmount`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.RaiseToAmount 80 |> Some)

  [<Theory>]
  [<InlineData(30, 90)>]
  [<InlineData(34, 100)>]
  let ``Call/raise + Pet: flop donk RaiseToAmount b/w 2 BB and 50% preflop pot`` bet raise =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 80 + bet; VillainBet = bet }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.RaiseToAmount raise |> Some)

  [<Fact>]
  let ``Call/raise + Pet: flop donk RaiseToAmount more than 50% preflop pot`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with Pot = 120; VillainBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Call when donk is call`` () =
    let options = { defaultOptions with Donk = OnDonk.Call }
    let snapshot = { defaultFlop with Pot = 120; VillainBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Call/raise + Pet: allin donk is > 50% preflop stack`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with HeroStack = 540; VillainStack = 130; VillainBet = 210 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call/raise + Pet: call allin donk`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultFlop with HeroStack = 460; VillainStack = 0; VillainBet = 460; Pot = 540 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)
  
  [<Fact>]
  let ``Call donk based on equity`` () =
    let options = { defaultOptions with Donk = OnDonk.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Call all-in donk based on equity with EQ >= 26`` () =
    let options = { defaultOptions with Donk = OnDonk.CallEQ 26 }
    let snapshot = { defaultFlop with Pot = 290; VillainStack = 0; VillainBet = 200 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Some Action.Call)

  [<Fact>]
  let ``Fold donk based on equity`` () =
    let options = { defaultOptions with Donk = OnDonk.CallEQ 17 }
    let snapshot = { defaultFlop with Pot = 110; VillainBet = 30 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Some Action.Fold)

  [<Fact>]
  let ``Special condition 5: check raised with FD`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.StackOff }
    let snapshot = { defaultFlop with Pot = 237; VillainBet = 117; HeroBet = 40; VillainStack = 293 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet normal for value with effective stack 15bb or more`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultTurn with HeroStack = 210; VillainStack = 610 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 90 |> Some, actual)

  [<Fact>]
  let ``Turn: Cbet all-in for value with effective stack 14bb or less`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 50m; IfPreStackLessThan = 14 } }
    let snapshot = { defaultTurn with HeroStack = 177; VillainStack = 620 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet all-in for value with effective stack < 2 bets`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 75m; IfStackFactorLessThan = Some 2m; IfPreStackLessThan = 100 } }
    let snapshot = { defaultTurn with Pot = 160; HeroStack = 230; VillainStack = 610 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet all-in for value with < 79 remaining chips after bet`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 70m; IfRemainingChipsLessThan = 79 } }
    let snapshot = { defaultTurn with Pot = 400; HeroStack = 290; VillainStack = 310 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Turn: Cbet for bluff with effective stack 19bb or more`` () =
    let options = { defaultOptions with CbetFactor = Always 50m }
    let snapshot = { defaultTurn with HeroStack = 288; VillainStack = 532 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 90 |> Some, actual)

  [<Fact>]
  let ``Turn: Cbet allin for bluff with effective stack less than 18bb`` () =
    let options = { defaultOptions with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 62.5m; IfPreStackLessThan = 18 } }
    let snapshot = { defaultTurn with HeroStack = 257; VillainStack = 547 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Stack off small turn donk`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultTurn with HeroStack = 430; Pot = 140; VillainStack = 430; VillainBet = 40 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.RaiseToAmount 240 |> Some)

  [<Fact>]
  let ``Call on RaiseConditional when stack is low`` () =
    let options = { defaultOptions with Donk = OnDonk.RaiseConditional { Size = 2.2m; MinStackPotRatio = 0.6m } }
    let snapshot = { defaultTurn with HeroStack = 330; Pot = 340; VillainStack = 330; VillainBet = 140 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.Call |> Some)

  [<Fact>]
  let ``Raise on RaiseConditional when stack is deep`` () =
    let options = { defaultOptions with Donk = OnDonk.RaiseConditional { Size = 2.2m; MinStackPotRatio = 0.6m } }
    let snapshot = { defaultTurn with HeroStack = 380; Pot = 240; VillainStack = 380; VillainBet = 100 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(actual, Action.RaiseToAmount 220 |> Some)

  [<Fact>]
  let ``Makes no decision on undefined donk`` () =
    let snapshot = { defaultTurn with VillainBet = 100 }
    let options = { defaultOptions with Donk = OnDonk.Undefined }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Makes no decision on undefined cbet`` () =
    let options = { defaultOptions with CbetFactor = CBet.Undefined }
    let actual = Decision.decide [] defaultFlop [] options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Makes no decision on undefined check-raise`` () =
    let snapshot = { defaultTurn with VillainBet = 100; HeroBet = 40 }
    let options = { defaultOptions with CheckRaise = OnCheckRaise.Undefined }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(None, actual)

  [<Fact>]
  let ``Raise 2.5x on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 80 }
    let options = { defaultOptions with Donk = OnDonk.RaiseX 250 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 200 |> Some, actual)

  [<Fact>]
  let ``Raise to all-in on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 80; VillainStack = 190 }
    let options = { defaultOptions with Donk = OnDonk.RaiseX 250 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Fact>]
  let ``Raises small donkbet on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 30; Pot = 210 }
    let options = { defaultOptions with Donk = OnDonk.RaisePreDonkX 110 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some(Action.RaiseToAmount 200), actual)

  [<Fact>]
  let ``Raises gaydonk on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 30; Pot = 210 }
    let options = { defaultOptions with Donk = OnDonk.RaiseGay }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some(Action.RaiseToAmount 120), actual)

  [<Fact>]
  let ``Raises gaydonk to all-in on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 30; Pot = 210; HeroStack = 185 }
    let options = { defaultOptions with Donk = OnDonk.RaiseGay }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some(Action.AllIn), actual)

  [<Fact>]
  let ``Raise donk to AI on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 80 }
    let options = { defaultOptions with Donk = OnDonk.AllIn }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  let donkAction = { Action = Action.RaiseToAmount 90; Motivation = None; VsVillainBet = 90; Street = Turn; Source = null }

  [<Fact>]
  let ``Stack-off after donk-raise on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 350; HeroBet = 200; Pot = 620 }
    let options = { defaultOptions with DonkRaise = OnDonkRaise.StackOff }
    let actual = Decision.decide [] snapshot [donkAction] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Call EQ after donk-raise on turn`` () =
    let snapshot = { defaultTurn with VillainBet = 350; HeroBet = 200; Pot = 620 }
    let options = { defaultOptions with DonkRaise = OnDonkRaise.CallEQ 20 }
    let actual = Decision.decide [] snapshot [donkAction] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Allin on 3bet after donk`` () =
    let options = { defaultOptions with DonkRaise = OnDonkRaise.AllIn }
    let snapshot = { defaultTurn with HeroStack = 380; VillainStack = 300; VillainBet = 160; HeroBet = 90; Pot = 330 }
    let actual = Decision.decide [] snapshot [donkAction] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Stack off all-in on river`` () =
    let snapshot = { defaultRiver with Pot = 260; VillainBet = 100; HeroStack = 320; VillainStack = 420 }
    let options = { defaultOptions with Donk = OnDonk.ForValueStackOffX 250 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.AllIn, actual)

  [<Fact>]
  let ``Stack off 2.5x on river`` () =
    let snapshot = { defaultRiver with Pot = 240; VillainBet = 80; HeroStack = 320; VillainStack = 440 }
    let options = { defaultOptions with Donk = OnDonk.ForValueStackOffX 250 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 200 |> Some, actual)

  [<Fact>]
  let ``Call big donk on river`` () =
    let snapshot = { defaultRiver with Pot = 240; VillainBet = 80; HeroStack = 320; VillainStack = 440 }
    let options = { defaultOptions with Donk = OnDonk.CallRaisePet }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Some Action.Call, actual)

  [<Fact>]
  let ``Raise micro donk on river`` () =
    let snapshot = { defaultRiver with Pot = 230; VillainBet = 30; HeroStack = 320; VillainStack = 490 }
    let options = { defaultOptions with Donk = OnDonk.CallRaisePet }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 180 |> Some, actual)

  [<Fact>]
  let ``Raise small donk on river`` () =
    let snapshot = { defaultRiver with Pot = 230; VillainBet = 70; HeroStack = 320; VillainStack = 450 }
    let options = { defaultOptions with Donk = OnDonk.CallRaisePet }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.RaiseToAmount 175 |> Some, actual)

  [<Fact>]
  let ``Call small check-raise on river`` () =
    let snapshot = { defaultRiver with Pot = 820; VillainBet = 430; HeroBet = 160; HeroStack = 90; VillainStack = 0 }
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 13 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.Call |> Some, actual)

  [<Fact>]
  let ``Fold all-in-check-raise on flop when villain had more chips`` () =
    let snapshot = { defaultFlop with Pot = 570; VillainStack = 0; HeroStack = 130; VillainBet = 685; HeroBet = 125 }
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 20 }
    let actual = Decision.decide [] snapshot [] options
    Assert.Equal(Action.Fold |> Some, actual)

  let riverBetSizing = 
    [{ MinPotSize = 0; MaxPotSize = 500; F1RRRatio = 2m; RTVRatio = 2.5m }]

  [<Fact>]
  let ``Formula raise on river`` () =
    let snapshot = { defaultRiver with Pot = 250; VillainBet = 100; VillainStack = 425; HeroStack = 425 }
    let options = { defaultOptions with Donk = OnDonk.FormulaRaise }
    let actual = Decision.decide riverBetSizing snapshot [] options
    Assert.Equal(Action.RaiseToAmount 300 |> Some, actual)

  let rbsStub = [], [], []

  [<Fact>]
  let ``Check flop OOP`` () =
    let snapshot = defaultFlop
    let options = defaultOopOptions
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Check |> Some, actual)

  [<Fact>]
  let ``Donk flop OOP`` () =
    let snapshot = defaultFlop
    let options = { defaultOopOptions with First = Donk(62.5m) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 50 |> Some, actual)

  [<Fact>]
  let ``Fold flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 50 }
    let options = defaultOopOptions
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Fold |> Some, actual)

  [<Fact>]
  let ``Stack Off: Check/raise flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 40; Pot = 120 }
    let options = { defaultOopOptions with Then = StackOff }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 110 |> Some, actual)

  [<Fact>]
  let ``Stack Off: Check/raise 1BB flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 20; Pot = 100 }
    let options = { defaultOopOptions with Then = StackOff }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 100 |> Some, actual)

  [<Fact>]
  let ``Stack Off: Check/raise limped flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 30; Pot = 70 }
    let options = { defaultOopOptions with Then = StackOff }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 90 |> Some, actual)

  [<Fact>]
  let ``Stack Off: Check/raise flop OOP to AllIn`` () =
    let snapshot = { defaultFlop with VillainBet = 70; Pot = 150; HeroStack = 290 }
    let options = { defaultOopOptions with Then = StackOff }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Fact>]
  let ``Stack Off+: Check/raise flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 40; Pot = 120 }
    let options = { defaultOopOptions with Then = StackOffFast }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 140 |> Some, actual)

  [<Fact>]
  let ``Stack Off+: Check/raise 1BB flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 20; Pot = 100 }
    let options = { defaultOopOptions with Then = StackOffFast }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 120 |> Some, actual)

  [<Fact>]
  let ``Stack Off+: Check/raise limped flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 30; Pot = 70 }
    let options = { defaultOopOptions with Then = StackOffFast }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 120 |> Some, actual)

  [<Fact>]
  let ``Stack Off gay: Check/raise gay bet`` () =
    let snapshot = { defaultFlop with VillainBet = 30; Pot = 100 }
    let options = { defaultOopOptions with Then = StackOffGay }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 65 |> Some, actual)

  [<Fact>]
  let ``Stack Off gay: Check/raise big bet`` () =
    let snapshot = { defaultFlop with VillainBet = 40; Pot = 110 }
    let options = { defaultOopOptions with Then = StackOffGay }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 110 |> Some, actual)

  [<Fact>]
  let ``Stack Off gay: AI on 3bet`` () =
    let snapshot = { defaultFlop with HeroBet = 65; VillainBet = 130; Pot = 265 }
    let options = { defaultOopOptions with Then = StackOffGay }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Fact>]
  let ``Raise/call gay: Check/raise gay bet`` () =
    let snapshot = { defaultFlop with VillainBet = 30; Pot = 100 }
    let options = { defaultOopOptions with Then = RaiseGayCallEQ 10 }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 65 |> Some, actual)

  [<Fact>]
  let ``Raise/fold: Check/raise flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 40; Pot = 120 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.Fold) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 110 |> Some, actual)

  [<Fact>]
  let ``Raise/call: Check/raise flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 40; Pot = 120 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.Call) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 110 |> Some, actual)

  [<Fact>]
  let ``Raise/call EQ: Check/raise 1BB flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 20; Pot = 100 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.CallEQ 20) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 80 |> Some, actual)

  [<Fact>]
  let ``Raise/fold: Fold 3bet flop OOP`` () =
    let snapshot = { defaultFlop with HeroBet = 110; VillainBet = 240; Pot = 360 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.Fold) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Fold |> Some, actual)

  [<Fact>]
  let ``Raise/call: Call 3bet flop OOP`` () =
    let snapshot = { defaultFlop with HeroBet = 110; VillainBet = 240; Pot = 360 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.Call) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Call |> Some, actual)

  [<Fact>]
  let ``Raise/call EQ: Fold 3bet flop OOP`` () =
    let snapshot = { defaultFlop with HeroBet = 110; VillainBet = 240; Pot = 360 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.CallEQ 26) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Fold |> Some, actual)

  [<Fact>]
  let ``Raise/call EQ: Call 3bet flop OOP`` () =
    let snapshot = { defaultFlop with HeroBet = 110; VillainBet = 240; Pot = 360 }
    let options = { defaultOopOptions with Then = Raise(2.75m, OopOnCBet.CallEQ 27) }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Call |> Some, actual)

  [<Fact>]
  let ``Raise gay/call EQ: Call 3bet flop OOP`` () =
    let snapshot = { defaultFlop with HeroBet = 110; VillainBet = 240; Pot = 360 }
    let options = { defaultOopOptions with Then = RaiseGayCallEQ 27 }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Call |> Some, actual)

  [<Fact>]
  let ``Check/AllIn: AllIn flop OOP`` () =
    let snapshot = { defaultFlop with VillainBet = 40 }
    let options = { defaultOopOptions with Then = AllIn }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Fact>]
  let ``Donk: Donk flop OOP`` () =
    let options = { defaultOopOptions with First = Donk 50m }
    let actual = Decision.decideOop rbsStub defaultFlop options
    Assert.Equal(Action.RaiseToAmount 40 |> Some, actual)

  [<Fact>]
  let ``Donk: Donk flop OOP AI if bet is > 80% stack`` () =
    let snapshot = { defaultFlop with Pot = 200; HeroStack = 149 }
    let options = { defaultOopOptions with First = Donk 60m }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Theory>]
  [<InlineData(20, 0, false)>]
  [<InlineData(15, 0, true)>]
  [<InlineData(50, 20, false)>]
  [<InlineData(40, 20, true)>]
  let ``Call If Raised OOP scenarios`` vb hb call =
    let snapshot = { defaultFlop with Pot = 100; VillainBet = vb; HeroBet = hb  }
    let options = { defaultOopOptions with Then = CallEQIfRaised(20, 15) }
    let actual = Decision.decideOop rbsStub snapshot options
    let expected = if call then Action.Call else Action.Fold
    Assert.Equal(expected |> Some, actual)

  let riverRules = 
    ([
      { MinPotSize = 0;   MaxPotSize = 199; MinAllInPercentage = 40; MaxAllInPercentage = 100; MinChipsLeft = 50; BetSize = 55 };
      { MinPotSize = 200; MaxPotSize = 500; MinAllInPercentage = 50; MaxAllInPercentage = 70;  MinChipsLeft = 60; BetSize = 60}]
    ,[
      { MinPotSize = 0;   MaxPotSize = 199; BetSize = 45; ThinBetSize = 35 }
      { MinPotSize = 200; MaxPotSize = 500; BetSize = 50; ThinBetSize = 40 }]
    ,[])

  let riverBetSizeTest rb stack expected =
    let snapshot = { defaultRiver with Pot = 200; HeroStack = stack }
    let options = { defaultOopOptions with First = rb }
    let actual = Decision.decideOop riverRules snapshot options
    Assert.Equal(expected |> Some, actual)

  [<Fact>]
  let ``River Bet Size Donk OOP check if stack is too low`` () =
    riverBetSizeTest RiverBetSizing 95 Action.Check

  [<Fact>]
  let ``River Bet Size Donk OOP AI if stack is in range`` () =
    riverBetSizeTest RiverBetSizing 105 Action.AllIn

  [<Fact>]
  let ``River Bet Size Donk OOP AI if stack is high but not enough chips will be behind`` () =
    riverBetSizeTest RiverBetSizing 150 Action.AllIn

  [<Fact>]
  let ``River Bet Size Donk OOP bets in stack is deep`` () =
    riverBetSizeTest RiverBetSizing 190 (Action.RaiseToAmount 120)

  [<Fact>]
  let ``River Bet Value AI if stack is high but not enough chips will be behind`` () =
    riverBetSizeTest RiverBetValue 175 Action.AllIn

  [<Fact>]
  let ``River Bet Value bets in stack is deep`` () =
    riverBetSizeTest RiverBetValue 185 (Action.RaiseToAmount 100)

  [<Fact>]
  let ``River Bet thin Value AI if stack is high but not enough chips will be behind`` () =
    riverBetSizeTest RiverBetThinValue 155 Action.AllIn

  [<Fact>]
  let ``River Bet thin Value bets in stack is deep`` () =
    riverBetSizeTest RiverBetThinValue 165 (Action.RaiseToAmount 80)

  [<Fact>]
  let ``Formula raise river`` () =
    let snapshot = { defaultRiver with Pot = 200; VillainStack = 300; HeroStack = 400; VillainBet = 100 }
    let options = { defaultOopOptions with Then = FormulaRaise { Or = StackOff; On3Bet = StackOff } }
    let actual = Decision.decideOop ([], [], riverBetSizing) snapshot options
    Assert.Equal(Action.RaiseToAmount 275 |> Some, actual)

  [<Fact>]
  let ``Formula raise river AI`` () =
    let snapshot = { defaultRiver with Pot = 200; VillainStack = 280; HeroStack = 400; VillainBet = 120 }
    let options = { defaultOopOptions with Then = FormulaRaise { Or = StackOff; On3Bet = StackOff } }
    let actual = Decision.decideOop ([], [], riverBetSizing) snapshot options
    Assert.Equal(Action.AllIn |> Some, actual)

  [<Fact>]
  let ``Formula raise river call 3bet`` () =
    let snapshot = { defaultRiver with Pot = 600; VillainStack = 150; HeroStack = 250; VillainBet = 300; HeroBet = 150 }
    let options = { defaultOopOptions with Then = FormulaRaise { Or = CallEQ 20; On3Bet = CallEQ 20 } }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Call |> Some, actual)

  [<Fact>]
  let ``Check/Raise turn with min stack pot ratio limits`` () =
    let snapshot = { defaultTurn with Pot = 160; VillainStack = 340; HeroStack = 400; VillainBet = 60 }
    let options = { defaultOopOptions with Then = RaiseConditional { Size = 2.2m; MinStackRemaining = 0; MinStackPotRatio = 0.55m; On3Bet = CallEQ 15 } }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 130 |> Some, actual)

  [<Fact>]
  let ``No Check/Raise turn with min stack pot ratio limits`` () =
    let snapshot = { defaultTurn with Pot = 220; VillainStack = 340; HeroStack = 440; VillainBet = 100 }
    let options = { defaultOopOptions with Then = RaiseConditional { Size = 2.2m; MinStackRemaining = 0; MinStackPotRatio = 0.55m; On3Bet = CallEQ 15 } }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Fold |> Some, actual)

  [<Fact>]
  let ``Check/Raise turn with min stack remaining limits`` () =
    let snapshot = { defaultTurn with Pot = 160; VillainStack = 340; HeroStack = 400; VillainBet = 60 }
    let options = { defaultOopOptions with Then = RaiseConditional { Size = 2.9m; MinStackRemaining = 100; MinStackPotRatio = 0m; On3Bet = CallEQ 15 } }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.RaiseToAmount 175 |> Some, actual)

  [<Fact>]
  let ``No Check/Raise turn with min stack remaining limits`` () =
    let snapshot = { defaultTurn with Pot = 220; VillainStack = 280; HeroStack = 500; VillainBet = 100 }
    let options = { defaultOopOptions with Then = RaiseConditional { Size = 2.9m; MinStackRemaining = 100; MinStackPotRatio = 0m; On3Bet = CallEQ 15 } }
    let actual = Decision.decideOop rbsStub snapshot options
    Assert.Equal(Action.Fold |> Some, actual)

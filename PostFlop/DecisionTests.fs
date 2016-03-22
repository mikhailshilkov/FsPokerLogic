namespace PostFlop

module DecisionTests =

  open Decision
  open Xunit

  let defaultOptions = { CbetFactor = None; CheckRaise = Call; Donk = FVStackOff  }
  let defaultSnapshot = { Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }

  [<Theory>]
  [<InlineData(80, 50, 40)>]
  [<InlineData(120, 25, 30)>]
  let ``Condition 1: CBet IP on flop if size is defined`` pot f cbet =
    let options = { defaultOptions with CbetFactor = Some f }
    let snapshot = { defaultSnapshot with Pot = pot }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Bet(cbet))

  [<Fact>]
  let ``Condition 2: No CBet IP on flop if size is undefined`` () =
    let options = { defaultOptions with CbetFactor = None }
    let expected = Decision.decide defaultSnapshot options
    Assert.Equal(expected, Check)

  [<Theory>]
  [<InlineData(100, 225)>]
  [<InlineData(110, 245)>]
  [<InlineData(90, 200)>]
  let ``Condition 3: Stack off on check-raise`` raise reraise =
    let options = { defaultOptions with CheckRaise = StackOff }
    let snapshot = { defaultSnapshot with VillainBet = raise; HeroBet = 40 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Bet(reraise))

  [<Fact>]
  let ``Condition 4: Call on check-raise`` () =
    let options = { defaultOptions with CheckRaise = Call }
    let snapshot = { defaultSnapshot with VillainBet = 100; HeroBet = 40 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Call)

  [<Fact>]
  let ``All-in on check-raise`` () =
    let options = { defaultOptions with CheckRaise = AllIn }
    let snapshot = { defaultSnapshot with VillainBet = 100; HeroBet = 40 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.AllIn)

  [<Fact>]
  let ``Call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 200; VillainBet = 80; HeroBet = 40 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Call)

  [<Fact>]
  let ``Fold check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Fold)

  [<Fact>]
  let ``FV & stack off flop donk bet`` () =
    let options = { defaultOptions with Donk = FVStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 140; VillainStack = 430; VillainBet = 60 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Bet 150)

  [<Fact>]
  let ``FV & stack off 1 BB flop donk bet`` () =
    let options = { defaultOptions with Donk = FVStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 100; VillainStack = 470; VillainBet = 20 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Bet 80)

  [<Fact>]
  let ``FV & stack off flop donk bet not smaller than minimal`` () =
    let options = { defaultOptions with Donk = FVStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 140; VillainStack = 410; VillainBet = 80 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Bet 180)

  [<Theory>]
  [<InlineData(20)>]
  [<InlineData(30)>]
  let ``Call/raise + Pet 1 BB flop donk bet`` bet =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 100; VillainStack = 490 - bet; VillainBet = bet }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Bet (bet * 4))

  [<Fact>]
  let ``Call/raise + Pet flop donk bet b/w 2 BB and 50% preflop pot`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 100; VillainStack = 456; VillainBet = 34 }
    let expected = Decision.decide snapshot options
    Assert.Equal(expected, Action.Bet 100)
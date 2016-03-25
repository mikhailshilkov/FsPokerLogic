namespace PostFlop

module DecisionTests =

  open Decision
  open Xunit

  let defaultOptions = { CbetFactor = None; CheckRaise = Call; Donk = ForValueStackOff  }
  let defaultSnapshot = { Pot = 80; VillainStack = 490; HeroStack = 430; VillainBet = 0; HeroBet = 0; BB = 20 }

  [<Theory>]
  [<InlineData(80, 50, 40)>]
  [<InlineData(120, 25, 30)>]
  let ``Condition 1: CBet IP on flop if size is defined`` pot f cbet =
    let options = { defaultOptions with CbetFactor = Some f }
    let snapshot = { defaultSnapshot with Pot = pot }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Bet(cbet))

  [<Fact>]
  let ``Condition 2: No CBet IP on flop if size is undefined`` () =
    let options = { defaultOptions with CbetFactor = None }
    let actual = Decision.decide defaultSnapshot options
    Assert.Equal(actual, Check)

  [<Theory>]
  [<InlineData(100, 225)>]
  [<InlineData(110, 245)>]
  [<InlineData(90, 200)>]
  let ``Condition 3: Stack off on check-raise`` raise reraise =
    let options = { defaultOptions with CheckRaise = StackOff }
    let snapshot = { defaultSnapshot with VillainBet = raise; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Bet(reraise))

  [<Fact>]
  let ``Condition 4: Call on check-raise`` () =
    let options = { defaultOptions with CheckRaise = Call }
    let snapshot = { defaultSnapshot with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Call)

  [<Fact>]
  let ``All-in on check-raise`` () =
    let options = { defaultOptions with CheckRaise = AllIn }
    let snapshot = { defaultSnapshot with VillainBet = 100; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.AllIn)

  [<Fact>]
  let ``Call check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 200; VillainBet = 80; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Call)

  [<Fact>]
  let ``Fold check-raise based on equity`` () =
    let options = { defaultOptions with CheckRaise = OnCheckRaise.CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 240; VillainBet = 120; HeroBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Fold)

  [<Fact>]
  let ``FV & stack off flop donk bet`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 140; VillainStack = 430; VillainBet = 60 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Bet 150)

  [<Fact>]
  let ``FV & stack off 1 BB flop donk bet`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 100; VillainStack = 470; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Bet 80)

  [<Fact>]
  let ``FV & stack off flop donk bet not smaller than minimal`` () =
    let options = { defaultOptions with Donk = ForValueStackOff }
    let snapshot = { defaultSnapshot with HeroStack = 430; Pot = 140; VillainStack = 410; VillainBet = 80 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Bet 180)

  [<Fact>]
  let ``Call/raise + Pet: 1 BB flop donk bet`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultSnapshot with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Bet 80)

  [<Theory>]
  [<InlineData(30, 90)>]
  [<InlineData(34, 100)>]
  let ``Call/raise + Pet: flop donk bet b/w 2 BB and 50% preflop pot`` bet raise =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultSnapshot with Pot = 80 + bet; VillainBet = bet }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Bet raise)

  [<Fact>]
  let ``Call/raise + Pet: flop donk bet more than 50% preflop pot`` () =
    let options = { defaultOptions with Donk = CallRaisePet }
    let snapshot = { defaultSnapshot with Pot = 120; VillainBet = 40 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Call)
  
  [<Fact>]
  let ``Call donk based on equity`` () =
    let options = { defaultOptions with Donk = CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 100; VillainBet = 20 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Call)

  [<Fact>]
  let ``Fold donk based on equity`` () =
    let options = { defaultOptions with Donk = CallEQ 17 }
    let snapshot = { defaultSnapshot with Pot = 110; VillainBet = 30 }
    let actual = Decision.decide snapshot options
    Assert.Equal(actual, Action.Fold)
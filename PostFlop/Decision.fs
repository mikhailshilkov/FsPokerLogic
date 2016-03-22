namespace PostFlop

module Decision =

  type Action = 
    | Fold
    | Check
    | Call
    | Bet of int
    | AllIn

  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int
  type OnDonk = FVStackOff | CallRaisePet

  type Options = {
    CbetFactor: int option
    CheckRaise: OnCheckRaise
    Donk: OnDonk
  }

  type Snapshot = {
    Pot: int 
    VillainStack: int 
    HeroStack: int 
    VillainBet: int 
    HeroBet: int
    BB: int
  }

  let roundTo5 v = (v + 2) / 5 * 5

  let cbet pot cbetf = pot * cbetf / 100

  let reraise villainBet = villainBet * 9 / 4 |> roundTo5

  let callEQ heroBet villainBet pot threshold = 
    if (villainBet - heroBet) * 100 / (pot + heroBet) < threshold
    then Action.Call
    else Action.Fold

  let stackOffDonk s =
    if s.VillainBet <= s.BB * 3 / 2 then 
      4 * s.VillainBet |> Bet
    else
      let potPre = s.Pot - s.HeroBet - s.VillainBet
      let stackPre = potPre / 2 + min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
      let raiseSize = (stackPre - 85 * potPre / 100) * 10 / 27
      max raiseSize (9 * s.VillainBet / 4) |> roundTo5 |> Bet

  let raisePetDonk s =
    if s.VillainBet <= s.BB * 3 / 2 then 
      4 * s.VillainBet |> Bet
    else
      3 * s.VillainBet |> roundTo5 |> Bet

  let decide snapshot options =
    if snapshot.VillainBet > 0 && snapshot.HeroBet = 0 then
      match options.Donk with
      | FVStackOff -> stackOffDonk snapshot
      | CallRaisePet -> raisePetDonk snapshot
    else if snapshot.VillainBet > 0 && snapshot.HeroBet > 0 then
      match options.CheckRaise with
      | StackOff -> reraise snapshot.VillainBet |> Bet
      | CallEQ eq -> callEQ snapshot.HeroBet snapshot.VillainBet snapshot.Pot eq
      | Call -> Action.Call
      | AllIn -> Action.AllIn
    else
      match options.CbetFactor with
      | Some f -> cbet snapshot.Pot f |> Bet
      | None -> Check

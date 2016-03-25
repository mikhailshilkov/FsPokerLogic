namespace PostFlop

module Decision =

  type Action = 
    | Fold
    | Check
    | Call
    | Bet of int
    | AllIn

  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int
  type OnDonk = ForValueStackOff | CallRaisePet | CallEQ of int

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
    let betToMake = villainBet - heroBet
    if betToMake * 100 / (pot + betToMake) < threshold
    then Action.Call
    else Action.Fold

  let stackOffDonk s =
    if s.VillainBet = s.BB then 
      4 * s.VillainBet |> Bet
    else
      let potPre = s.Pot - s.HeroBet - s.VillainBet
      let stackPre = potPre / 2 + min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
      let raiseSize = (stackPre - 85 * potPre / 100) * 10 / 27
      max raiseSize (9 * s.VillainBet / 4) |> roundTo5 |> Bet

  let raisePetDonk s =
    if s.VillainBet = s.BB then 
      4 * s.VillainBet |> Bet
    else 
      let potPre = s.Pot - s.HeroBet - s.VillainBet
      if s.VillainBet < potPre / 2 then
        3 * s.VillainBet |> roundTo5 |> Bet
      else
        Action.Call

  let decide snapshot options =
    if snapshot.VillainBet > 0 && snapshot.HeroBet = 0 then
      match options.Donk with
      | ForValueStackOff -> stackOffDonk snapshot
      | CallRaisePet -> raisePetDonk snapshot
      | CallEQ eq -> callEQ 0 snapshot.VillainBet snapshot.Pot eq
    else if snapshot.VillainBet > 0 && snapshot.HeroBet > 0 then
      match options.CheckRaise with
      | StackOff -> reraise snapshot.VillainBet |> Bet
      | OnCheckRaise.CallEQ eq -> callEQ snapshot.HeroBet snapshot.VillainBet snapshot.Pot eq
      | Call -> Action.Call
      | AllIn -> Action.AllIn
    else
      match options.CbetFactor with
      | Some f -> cbet snapshot.Pot f |> Bet
      | None -> Check

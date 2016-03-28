namespace PostFlop

open Options

module Decision =

  type Action = 
    | Fold
    | Check
    | Call
    | Bet of int
    | AllIn

  type Snapshot = {
    Pot: int 
    VillainStack: int 
    HeroStack: int 
    VillainBet: int 
    HeroBet: int
    BB: int
  }

  let roundTo5 v = (v + 2) / 5 * 5
  let potPre s = s.Pot - s.HeroBet - s.VillainBet
  let betPre s = (potPre s) / 2
  let stackPre s = betPre s + min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
  let callSize s = s.VillainBet - s.HeroBet
  let stackIfCall s = min (s.HeroStack - (callSize s)) s.VillainStack
  let potOdds s = (callSize s) * 100 / (s.Pot + (callSize s))

  let cbet pot cbetf = pot * cbetf / 100

  let reraise s = 
    let size = s.VillainBet * 9 / 4 |> roundTo5 
    if size > (stackPre s) * 53 / 100 then Action.AllIn
    else size |> Bet

  let callraise s =
    if (stackIfCall s) * 5 < (s.Pot + (callSize s)) 
    then Action.AllIn
    else Action.Call

  let callEQ s threshold = 
    if potOdds s < threshold then Action.Call else Action.Fold

  let stackOffDonk s =
    if s.VillainBet = s.BB then 
      4 * s.VillainBet |> Bet
    else
      let raiseSize = ((stackPre s) - 85 * (potPre s) / 100) * 10 / 27
      max raiseSize (9 * s.VillainBet / 4) |> roundTo5 |> Bet

  let raisePetDonk s =
    if s.VillainBet = s.BB then 
      4 * s.VillainBet |> Bet
    else 
      if s.VillainBet < betPre s then
        3 * s.VillainBet |> roundTo5 |> Bet
      else if s.VillainBet * 2 > stackPre s then Action.AllIn
      else Action.Call

  let decide snapshot options =
    if snapshot.VillainBet > 0 && snapshot.HeroBet = 0 then
      match options.Donk with
      | ForValueStackOff -> stackOffDonk snapshot
      | CallRaisePet -> raisePetDonk snapshot
      | CallEQ eq -> 
        let modifiedEq = if snapshot.VillainStack = 0 && eq >= 26 then eq + 15 else eq
        callEQ snapshot modifiedEq
      | Undefined -> failwith "Donk behavior is not defined"
    else if snapshot.VillainBet > 0 && snapshot.HeroBet > 0 then
      match options.CheckRaise with
      | OnCheckRaise.StackOff -> reraise snapshot
      | OnCheckRaise.CallEQ eq -> callEQ snapshot eq
      | OnCheckRaise.Call -> callraise snapshot
      | OnCheckRaise.AllIn -> Action.AllIn
      | OnCheckRaise.Undefined -> failwith "Check/raise behavior is not defined"
    else
      match options.CbetFactor with
      | Some f -> cbet snapshot.Pot f |> Bet
      | None -> Check

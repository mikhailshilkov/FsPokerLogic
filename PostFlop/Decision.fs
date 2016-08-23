namespace PostFlop

open Cards.Actions
open Hands
open Options

module Decision =

  type Snapshot = {
    Pot: int 
    VillainStack: int 
    HeroStack: int 
    VillainBet: int 
    HeroBet: int
    BB: int
    Hand: SuitedHand
    Board: Board
  }

  let street s = 
    match s.Board.Length with
    | 5 -> River
    | 4 -> Turn 
    | 3 -> Flop
    | 0 -> PreFlop
    | _ -> failwith "Weird board length"

  let streetIndex s = 
    match s with
    | River -> 5
    | Turn -> 4
    | Flop -> 3
    | PreFlop -> 1

  let boardAtStreet street board = 
    let indx = min (streetIndex street) (Array.length board)
    Array.take indx board

  let roundTo5 v = (v + 2) / 5 * 5
  let potPre s = s.Pot - s.HeroBet - s.VillainBet
  let pot s = s.Pot - max 0 (s.VillainBet - s.HeroBet - s.HeroStack)
  let betPre s = (potPre s) / 2
  let stackPre s = betPre s + min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
  let stack s = min s.HeroStack s.VillainStack
  let effectiveStackOnCurrentStreet s = min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
  let effectiveStackPre s = (stackPre s + s.BB / 2 - 1) / s.BB
  let callSize s = min (s.VillainBet - s.HeroBet) s.HeroStack
  let stackIfCall s = min (s.HeroStack - (callSize s)) s.VillainStack
  let potOdds s = (callSize s |> decimal) * 100m / (pot s + (callSize s) |> decimal) |> ceil |> int
  let times i d = ((i |> decimal) * d) |> int
  let wasRaisedPre s = betPre s > s.BB
  let relativeBet s = s.VillainBet * 100 / (s.Pot - s.VillainBet)

  let cbet pot cbetf = (pot |> decimal) * cbetf / 100m |> int

  let cbetOr s f defaultAction =
    let size = cbet s.Pot f.Factor
    if times size (defaultArg f.IfStackFactorLessThan 0m) < stack s
      && effectiveStackPre s > f.IfPreStackLessThan 
    then size |> RaiseToAmount
    else defaultAction

  let reraise s = 
    let size = s.VillainBet * 9 / 4 |> roundTo5 
    if size > (stackPre s) * 53 / 100 then Action.AllIn
    else size |> RaiseToAmount

  let callraise s =
    if (stackIfCall s) * 5 < (s.Pot + (callSize s)) 
    then Action.AllIn
    else Action.Call

  let callEQ s threshold = 
    if potOdds s <= threshold then Action.Call else Action.Fold

  let preventMicroRaises s d =
    if match street s with | Turn | River -> true | _ -> false 
      && s.VillainBet <= 3 * s.BB && s.VillainBet * 7 / 20 < s.Pot 
    then 6 * s.VillainBet 
    else if s.VillainBet = s.BB then 4 * s.VillainBet
    else d

  let orAllIn threshold s action =
    match action with
    | RaiseToAmount x when x + threshold > effectiveStackOnCurrentStreet s -> Action.AllIn
    | _ -> action

  let stackOffDonkX x s = 
    let raiseSize = s.VillainBet * x / 100 |> preventMicroRaises s
    Action.RaiseToAmount raiseSize |> orAllIn 100 s

  let callRaiseRiver s =
    if s.VillainBet * 2 < (s.Pot - s.VillainBet) then stackOffDonkX 250 s
    else Action.Call

  let stackOffDonk s =
    let calculatedRaiseSize = ((stackPre s) - 85 * (potPre s) / 100) * 10 / 27 |> preventMicroRaises s
    let raiseSize =
      if calculatedRaiseSize > s.VillainBet * 2 then calculatedRaiseSize
      else (9 * s.VillainBet / 4)
      |> roundTo5 
    RaiseToAmount raiseSize |> orAllIn 69 s

  let raisePetDonk s =
    if s.VillainBet < betPre s then
      3 * s.VillainBet |> preventMicroRaises s |> roundTo5 |> RaiseToAmount
    else if s.VillainBet * 2 > stackPre s && s.VillainStack > 0 then Action.AllIn
    else Action.Call

  let raisePreDonk x s =
    (s.Pot - s.VillainBet) * 11 / 10 |> roundTo5 |> RaiseToAmount |> orAllIn 69 s

  let raiseGay s =
    (s.VillainBet + s.Pot) / 2 |> roundTo5 |> RaiseToAmount |> orAllIn 69 s

  let ensureMinRaise s a =
    match a with
    | RaiseToAmount x when x <= s.BB -> MinRaise
    | x -> x

  let decide snapshot history options =
    if snapshot.VillainBet > 0 && snapshot.HeroBet = 0 then
      match options.Donk, street snapshot with
      | ForValueStackOffX(x), _ | RaiseX(x), _ -> stackOffDonkX x snapshot |> Some
      | ForValueStackOff, _ -> stackOffDonk snapshot |> Some
      | RaisePreDonkX(x), _ -> raisePreDonk x snapshot |> Some
      | RaiseGay, _ -> raiseGay snapshot |> Some
      | CallRaisePet, River -> callRaiseRiver snapshot |> Some
      | CallRaisePet, _ -> raisePetDonk snapshot |> Some
      | OnDonk.CallEQ eq, _ -> 
        let modifiedEq = if snapshot.VillainStack = 0 && eq >= 26 then eq + 15 else eq
        callEQ snapshot modifiedEq |> Some
      | OnDonk.AllIn, _ -> Some Action.AllIn
      | OnDonk.Call, _ -> Some Action.Call
      | OnDonk.Fold, _ -> Some Action.Fold
      | OnDonk.Undefined, _ -> None
    else if snapshot.VillainBet > 0 && snapshot.HeroBet > 0 then
      let raisedDonk = history |> List.tryLast |> Option.filter (fun a -> a.VsVillainBet > 0) |> Option.isSome
      if raisedDonk then
        match options.DonkRaise with
        | OnDonkRaise.CallEQ x -> callEQ snapshot x |> Some
        | OnDonkRaise.StackOff -> stackOffDonk snapshot |> Some
        | OnDonkRaise.Undefined -> None
      else
        match options.CheckRaise with
        | OnCheckRaise.StackOff -> reraise snapshot |> Some
        | OnCheckRaise.CallEQ eq -> callEQ snapshot eq |> Some
        | OnCheckRaise.Call -> callraise snapshot |> Some
        | OnCheckRaise.AllIn -> Some Action.AllIn
        | OnCheckRaise.Fold -> Some Action.Fold
        | OnCheckRaise.Undefined -> None
    else 
      match options.CbetFactor with
      | Always f -> cbet snapshot.Pot f |> RaiseToAmount |> Some
      | OrAllIn f -> cbetOr snapshot f Action.AllIn |> Some
      | Never -> Action.Check |> Some
      | CBet.Undefined -> None      
      |> Option.map (ensureMinRaise snapshot)

  let betXPot x s =
    let raiseSize = (s.Pot |> decimal) * x / 100m |> int |> roundTo5 
    if raiseSize * 5 / 4 > effectiveStackOnCurrentStreet s then Action.AllIn
    else raiseSize |> RaiseToAmount

  let raiseOop xlimped xraised xraisedonkbb s =
    let k = 
      if not (wasRaisedPre s) then xlimped
      else if s.VillainBet = s.BB then xraisedonkbb
      else xraised
    let raiseSize = s.VillainBet |> decimal |> (*) k |> int |> roundTo5 
    if raiseSize * 100 / 65 > effectiveStackOnCurrentStreet s then Action.AllIn
    else RaiseToAmount raiseSize

  let stackOffGay s =
    if s.HeroBet > 0 then Action.AllIn
    elif s.VillainBet < s.Pot / 3 then raiseGay s
    else s.VillainBet * 11 / 4 |> RaiseToAmount |> orAllIn 69 s

  let riverBetBluff riverBetSizes s =
    let rule = riverBetSizes |> List.filter (fun x -> x.MinPotSize <= s.Pot && s.Pot <= x.MaxPotSize) |> List.head
    let stack = effectiveStackOnCurrentStreet s * 100 / s.Pot
    if stack < rule.MinAllInPercentage then Action.Check
    elif stack < rule.MaxAllInPercentage then Action.AllIn
    else
      let betSize = rule.BetSize * s.Pot / 100 |> roundTo5
      betSize |> RaiseToAmount |> orAllIn rule.MinChipsLeft s

  let formulaRiverRaise s =
    let betSize = (7 * s.VillainBet / 2 + s.Pot) / 2 |> roundTo5
    let stack = effectiveStackOnCurrentStreet s
    if betSize > 3 * stack / 4 then Action.AllIn 
    else betSize |> RaiseToAmount


  let rec decideOop riverBetSizes s options =
    let rec onVillainBet a =
      let conditionalRaise x s = 
        let betSize = x.Size * (s.VillainBet |> decimal) |> int  |> roundTo5 
        let stackOnStreet = effectiveStackOnCurrentStreet s
        let potRateAfterCall = (stackOnStreet - betSize |> decimal) / (potPre s + 2 * betSize  |> decimal)
        if x.MinStackRemaining < stackOnStreet - betSize && x.MinStackPotRatio < potRateAfterCall
        then RaiseToAmount betSize
        else onVillainBet x.On3Bet
      match a with
      | StackOff -> raiseOop 3m 2.75m 5m s
      | StackOffFast -> raiseOop 4m 3.5m 6m s
      | StackOffGay -> stackOffGay s
      | RaiseFold x when s.HeroBet = 0 -> raiseOop x x 4m s
      | RaiseCall | RaiseCallEQ _ when s.HeroBet = 0 -> raiseOop 2.75m 2.75m 4m s
      | RaiseGayCallEQ _ when s.HeroBet = 0 -> stackOffGay s
      | FormulaRaise x -> if s.HeroBet > 0 then onVillainBet x else formulaRiverRaise s
      | Raise x -> if s.HeroBet > 0 then onVillainBet x.On3Bet else conditionalRaise x s
      | Fold | RaiseFold _ -> Action.Fold
      | Call | RaiseCall -> Action.Call
      | CallEQ i | RaiseCallEQ i | RaiseGayCallEQ i -> callEQ s i
      | AllIn -> Action.AllIn      

    if s.VillainBet = 0 then
      match options.First with
      | Check -> Action.Check
      | Donk x -> betXPot x s |> ensureMinRaise s
      | OopDonk.AllIn -> Action.AllIn
      | RiverBetSizing -> riverBetBluff riverBetSizes s
      |> Some
    elif s.VillainBet > 0 then
      onVillainBet options.Then |> Some
    else None
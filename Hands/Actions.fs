namespace Cards

module Actions =
  type Action = 
  | AllIn
  | MinRaise
  | RaiseToAmount of int
  | Call
  | Check
  | Fold
  | SitBack

  type Street = PreFlop | Flop | Turn | River

  type Motivation = Bluff

  type MotivatedAction = {
    Action: Action
    Motivation: Motivation option
    VsVillainBet: int
    Street: Street
  }

  let notMotivated street vb action = { Action = action; Motivation = None; VsVillainBet = vb; Street = street }
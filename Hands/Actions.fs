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

  type FloatType = ValueFloat | BluffFloat | WithContinuation of string

  type Motivation = 
    | Bluff
    | Scenario of string
    | Float of FloatType

  type MotivatedAction = {
    Action: Action
    Motivation: Motivation option
    VsVillainBet: int
    Street: Street
  }

  let notMotivated street vb action = { Action = action; Motivation = None; VsVillainBet = vb; Street = street }
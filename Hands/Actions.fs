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

  type Motivation = Bluff

  type MotivatedAction = {
    Action: Action
    Motivation: Motivation option
  }

  let notMotivated action = { Action = action; Motivation = None }
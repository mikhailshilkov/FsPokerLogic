namespace Cards

module Actions =
  type Action = 
  | AllIn
  //| MinRaise
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
    Source: string
  }

  let notMotivated street vb action = { Action = action; Motivation = None; VsVillainBet = vb; Street = street; Source = null }
  let bluff street vb hb = { Action = RaiseToAmount hb; Motivation = Some Bluff; VsVillainBet = vb; Street = street; Source = null }
  let scenario street vb action s = { Action = action; Motivation = (Some(Scenario(s))); VsVillainBet = vb; Street = street; Source = null }
  let floatBluff street vb = { Action = Call; Motivation = Some(Float BluffFloat); VsVillainBet = vb; Street = street; Source = null }
  let floatValue street vb = { Action = Call; Motivation = Some(Float ValueFloat); VsVillainBet = vb; Street = street; Source = null }
  let floatContinuation street vb action s = { Action = action; Motivation = Some(Float(WithContinuation s)); VsVillainBet = vb; Street = street; Source = null }
  let floatBluffCheck street = { Action = Check; Motivation = Some(Float BluffFloat); VsVillainBet = 0; Street = street; Source = null }
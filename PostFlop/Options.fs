namespace PostFlop

module Options =

  type CBetOr = {
    Factor: decimal
    IfStackFactorLessThan: decimal option
    IfPreStackLessThan: int
    IfRemainingChipsLessThan: int
  }
  let DefaultCBetOr = { Factor = 0m; IfStackFactorLessThan = None; IfPreStackLessThan = 0; IfRemainingChipsLessThan = 0 }
  type CBet = Always of decimal | OrAllIn of CBetOr | Never | Undefined
  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int | Fold | Undefined
  type OnDonk = ForValueStackOff | ForValueStackOffX of int | CallRaisePet | CallEQ of int | Call | Fold | Undefined

  type Options = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
  }

  type OopDonk = Check | Donk of decimal | AllIn
  type OopOnCBet = Fold | StackOff | StackOffFast | CallEQ of int | RaiseFold of decimal | RaiseCall | RaiseCallEQ of int | Call | AllIn
  type OopSpecialCondition = 
    | CallEQPlusXvsAI of int 
    | PairedBoard of OopDonk * OopOnCBet
    | BoardOvercard of OopDonk * OopOnCBet
    | BoardAce of OopDonk * OopOnCBet
    | CheckCheck of OopDonk * OopOnCBet
    | CheckCheckAndBoardOvercard of OopDonk * OopOnCBet
    | KHighOnPaired
    | NotUsed
  type OptionsOop = {
    First: OopDonk
    Then: OopOnCBet
    Special: OopSpecialCondition list
  }
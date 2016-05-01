namespace PostFlop

module Options =

  type CBetOr = {
    Factor: decimal
    IfStackFactorLessThan: decimal
    IfPreStackLessThan: int
  }
  type CBet = Always of decimal | OrAllIn of CBetOr | OrCheck of CBetOr | Never | Undefined
  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int | Fold | Undefined
  type OnDonk = ForValueStackOff | ForValueStackOffX of int | CallRaisePet | CallEQ of int | Call | Fold | Undefined

  type Options = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
  }

namespace PostFlop

module Options =

  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int | Undefined
  type OnDonk = ForValueStackOff | CallRaisePet | CallEQ of int | Undefined

  type Options = {
    CbetFactor: decimal option
    CheckRaise: OnCheckRaise
    Donk: OnDonk
  }

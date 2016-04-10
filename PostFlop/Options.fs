namespace PostFlop

module Options =

  type CBet = ForValue of decimal | ForBluff of decimal | NoCBet
  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int | Fold | Undefined
  type OnDonk = ForValueStackOff | CallRaisePet | CallEQ of int | Undefined

  type Options = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
  }

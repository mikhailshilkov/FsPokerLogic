namespace PostFlop

module Options =

  type OnCheckRaise = StackOff | Call | AllIn | CallEQ of int
  type OnDonk = ForValueStackOff | CallRaisePet | CallEQ of int

  type Options = {
    CbetFactor: int option
    CheckRaise: OnCheckRaise
    Donk: OnDonk
    DonkFlashDraw: OnDonk option
  }

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
  type OnDonk = 
    | ForValueStackOff 
    | ForValueStackOffX of int 
    | CallRaisePet 
    | CallEQ of int 
    | RaisePreDonkX of int
    | RaiseX of int 
    | RaiseGay //(2VB + P) / 2
    | AllIn 
    | Call 
    | Fold 
    | Undefined

  type OnDonkRaise = 
    | StackOff
    | CallEQ of int 
    | Undefined

  type Options = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
    DonkRaise: OnDonkRaise
  }

  type OopDonk = Check | Donk of decimal | AllIn | RiverBetSizing
  type OopOnCBet = 
    |Fold 
    | StackOff 
    | StackOffFast 
    | StackOffGay 
    | CallEQ of int 
    | RaiseFold of decimal 
    | RaiseCall 
    | RaiseCallEQ of int 
    | RaiseGayCallEQ of int 
    | FormulaRaise of OopOnCBet
    | Call 
    | AllIn

  type OopSpecialCondition = 
    | CallEQPlusXvsAI of int 
    | PairedBoard of OopDonk * OopOnCBet
    | BoardOvercard of OopDonk * OopOnCBet
    | BoardAce of OopDonk * OopOnCBet
    | CheckCheck of OopDonk * OopOnCBet
    | CheckCheckAndBoardOvercard of OopDonk * OopOnCBet
    | KHighOnPaired
    | CheckRaiseOvercardBluff of OopOnCBet
    | NotUsed

  type OptionsOop = {
    First: OopDonk
    Then: OopOnCBet
    Special: OopSpecialCondition list
    Scenario: string
    SpecialScenario: string
  }

  type RiverBetSizeEntry = {
    MinPotSize: int
    MaxPotSize: int
    MinAllInPercentage: int
    MaxAllInPercentage: int
    MinChipsLeft:int
    BetSize: int
  }

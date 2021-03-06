﻿namespace PostFlop

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
  type DonkConditionalRaise = { Size: decimal; MinStackPotRatio: decimal }
  type OnDonk = 
    | ForValueStackOff 
    | ForValueStackOffX of int 
    | CallRaisePet 
    | CallEQ of int 
    | CallEQvsAI of int * int
    | RaiseConditional of DonkConditionalRaise
    | RaisePreDonkX of int
    | RaiseX of int 
    | RaiseGay //(2VB + P) / 2
    | FormulaRaise
    | RaiseThinValue of OnDonk // Alternative action vs AI
    | AllIn 
    | Call 
    | Fold 
    | Undefined

  type OnDonkRaise = 
    | StackOff
    | AllIn
    | Call
    | CallEQ of int 
    | CallEQvsAI of int * int
    | Undefined

  type Options = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
    DonkRaise: OnDonkRaise
  }

  type OopDonk = Check | Donk of decimal | AllIn | RiverBetSizing | RiverBetValue | RiverBetThinValue
  type OopConditionalRaise = { Size: decimal; MinStackRemaining: int; MinStackPotRatio: decimal; On3Bet: OopOnCBet }
  and OopFormulaRaise = { Or: OopOnCBet; On3Bet: OopOnCBet }
  and OopOnCBet = 
    |Fold 
    | StackOff 
    | StackOffFast 
    | StackOffGay 
    | CallEQ of int 
    | CallEQIfRaised of int * int
    | Raise of decimal * OopOnCBet
    | RaiseGayCallEQ of int 
    | FormulaRaise of OopFormulaRaise
    | RaiseThinValue of OopFormulaRaise
    | RaiseConditional of OopConditionalRaise
    | Call 
    | AllIn

  type OopSpecialCondition = 
    | CallEQPlusXvsAI of int 
    | PairedBoard of OopDonk * OopOnCBet
    | BoardOvercard of OopDonk * OopOnCBet
    | BoardOvercardNotAce of OopDonk * OopOnCBet
    | BoardAce of OopDonk * OopOnCBet
    | CheckCheck of OopDonk * OopOnCBet
    | CheckCheckAndBoardOvercard of OopDonk * OopOnCBet
    | CheckCheckCheckCheck of OopDonk * OopOnCBet
    | KHighOnPaired
    | CheckRaiseOvercardBluff of OopOnCBet
    | SlowPlayedBefore of OopOnCBet
    | BarrelX3 of OopOnCBet
    | VillainRaised of OopDonk * OopOnCBet
    | HeroRaised of OopDonk * OopOnCBet
    | StackPotRatioLessThan of decimal * OopDonk * OopOnCBet
    | SmartyAllIn
    | NotUsed

  type OptionsOop = {
    First: OopDonk
    Then: OopOnCBet
    Scenario: string
    Special: (OopSpecialCondition * string) list
  }

  type RBSEntry = {
    MinPotSize: int
    MaxPotSize: int
    MinAllInPercentage: int
    MaxAllInPercentage: int
    MinChipsLeft: int
    BetSize: int
  }

  type RBVEntry = {
    MinPotSize: int
    MaxPotSize: int
    BetSize: int
    ThinBetSize: int
  }

  type F1RRRTVEntry = {
    MinPotSize: int
    MaxPotSize: int
    F1RRRatio: decimal
    RTVRatio: decimal
  }

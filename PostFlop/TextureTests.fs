module TextureTests

open Xunit
open PostFlop.Options
open PostFlop.Import
open PostFlop.Texture
open Hands

let defaultOptions = { 
  CbetFactor = Never
  CheckRaise = OnCheckRaise.Undefined
  Donk = OnDonk.Undefined
  DonkFlashDraw = None
  TurnFVCbetCards = ""
  TurnFVCbetFactor = Never
  TurnCheckRaise = OnCheckRaise.Undefined
  TurnFBCbetCards = ""
  TurnFBCbetFactor = Never 
  TurnFDCbetCards = ""
  TurnFDCbetFactor = Never}

[<Fact>]
let ``toFlopOptions when no FD`` () =
  let eo = { defaultOptions with CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false false false eo
  let expected = { Options.CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when no FD but board has FD and Cbet is 75m => Cbet becomes 100m`` () =
  let eo = { defaultOptions with CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false false true eo
  let expected = { Options.CbetFactor = Always 100m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD`` () =
  let eo = { defaultOptions with CbetFactor = Always 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions false true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD Cbets 50m instead of None and Stacks Off`` () =
  let eo = { defaultOptions with CbetFactor = Never; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions false true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD but Donk FD is not defined`` () =
  let eo = { defaultOptions with CbetFactor = Never; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = None }
  let actual = toFlopOptions false true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions on monoboard is not defined`` () =
  let actual = toFlopOptions true false false defaultOptions
  let expected = { Options.CbetFactor = CBet.Undefined; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Always 62.5m }
  let actual = toTurnOptions Face.Three false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions no Cbet not FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Always 62.5m }
  let actual = toTurnOptions Face.Five false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = Always 50m }
  let actual = toTurnOptions Face.Seven false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions no Cbet not FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = Always 50m }
  let actual = toTurnOptions Face.Eight false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFDCbetFactor = Always 70m }
  let actual = toTurnOptions Face.Eight false true OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 70m; CheckRaise = OnCheckRaise.AllIn; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions no Cbet not FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFVCbetFactor = Always 70m }
  let actual = toTurnOptions Face.Jack false true OnDonk.Undefined eo
  let expected = { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FV Check Raise`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "J"; TurnFVCbetFactor = Always 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Jack false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FB Check Raise`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "Q"; TurnFBCbetFactor = Always 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Queen false false OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FD Check Raise`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "A"; TurnFDCbetFactor = Always 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Ace false true OnDonk.Undefined eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.AllIn; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Flush`` () =
  let actual = toTurnOptions Face.Two true false OnDonk.Undefined defaultOptions
  let expected = { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)
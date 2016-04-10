module TextureTests

open Xunit
open PostFlop.Options
open PostFlop.Import
open PostFlop.Texture
open Hands

let defaultOptions = { 
  CbetFactor = NoCBet
  CheckRaise = OnCheckRaise.Undefined
  Donk = OnDonk.Undefined
  DonkFlashDraw = None
  TurnFVCbetCards = ""
  TurnFVCbetFactor = NoCBet
  TurnCheckRaise = OnCheckRaise.Undefined
  TurnFBCbetCards = ""
  TurnFBCbetFactor = NoCBet 
  TurnFDCbetCards = ""
  TurnFDCbetFactor = NoCBet}

[<Fact>]
let ``toFlopOptions when no FD`` () =
  let eo = { defaultOptions with CbetFactor = ForValue 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false false eo
  let expected = { Options.CbetFactor = ForValue 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when no FD but board has FD and Cbet is 75m => Cbet becomes 100m`` () =
  let eo = { defaultOptions with CbetFactor = ForValue 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false true eo
  let expected = { Options.CbetFactor = ForValue 100m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD`` () =
  let eo = { defaultOptions with CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD Cbets 50m instead of None and Stacks Off`` () =
  let eo = { defaultOptions with CbetFactor = NoCBet; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD but Donk FD is not defined`` () =
  let eo = { defaultOptions with CbetFactor = NoCBet; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = None }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = ForValue 62.5m }
  let actual = toTurnOptions Face.Three false false eo
  let expected = { Options.CbetFactor = ForValue 62.5m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = ForValue 62.5m }
  let actual = toTurnOptions Face.Five false false eo
  let expected = { Options.CbetFactor = NoCBet; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = ForBluff 50m }
  let actual = toTurnOptions Face.Seven false false eo
  let expected = { Options.CbetFactor = ForBluff 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FB card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "5,6,7"; TurnFVCbetFactor = ForValue 50m }
  let actual = toTurnOptions Face.Eight false false eo
  let expected = { Options.CbetFactor = NoCBet; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFDCbetFactor = ForValue 70m }
  let actual = toTurnOptions Face.Eight false true eo
  let expected = { Options.CbetFactor = ForValue 70m; CheckRaise = OnCheckRaise.AllIn; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFVCbetFactor = ForValue 70m }
  let actual = toTurnOptions Face.Jack false true eo
  let expected = { Options.CbetFactor = NoCBet; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FV Check Raise`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "J"; TurnFVCbetFactor = ForValue 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Jack false false eo
  let expected = { Options.CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FB Check Raise`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "Q"; TurnFBCbetFactor = ForBluff 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Queen false false eo
  let expected = { Options.CbetFactor = ForBluff 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FD Check Raise`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "A"; TurnFDCbetFactor = ForValue 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptions Face.Ace false true eo
  let expected = { Options.CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.AllIn; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Flush`` () =
  let actual = toTurnOptions Face.Two true false defaultOptions
  let expected = { Options.CbetFactor = ForValue 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)
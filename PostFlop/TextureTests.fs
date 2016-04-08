module TextureTests

open Xunit
open PostFlop.Options
open PostFlop.Import
open PostFlop.Texture
open Hands

let defaultOptions = { 
  CbetFactor = None
  CheckRaise = OnCheckRaise.Undefined
  Donk = OnDonk.Undefined
  DonkFlashDraw = None
  TurnFVCbetCards = ""
  TurnFVCbetFactor = None
  TurnFBCbetCards = ""
  TurnFBCbetFactor = None 
  TurnFDCbetCards = ""
  TurnFDCbetFactor = None}

[<Fact>]
let ``toFlopOptions when no FD`` () =
  let eo = { defaultOptions with CbetFactor = Some 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false false eo
  let expected = { Options.CbetFactor = Some 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when no FD but board has FD and Cbet is 75m => Cbet becomes 100m`` () =
  let eo = { defaultOptions with CbetFactor = Some 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false true eo
  let expected = { Options.CbetFactor = Some 100m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD`` () =
  let eo = { defaultOptions with CbetFactor = Some 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Some 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD Cbets 50m instead of None and Stacks Off`` () =
  let eo = { defaultOptions with CbetFactor = None; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Some 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD but Donk FD is not defined`` () =
  let eo = { defaultOptions with CbetFactor = None; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = None }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Some 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Some 62.5m }
  let actual = toTurnOptions Face.Three false eo
  let expected = { Options.CbetFactor = Some 62.5m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Some 62.5m }
  let actual = toTurnOptions Face.Five false eo
  let expected = { Options.CbetFactor = None; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = Some 50m }
  let actual = toTurnOptions Face.Seven false eo
  let expected = { Options.CbetFactor = Some 50m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FB card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "5,6,7"; TurnFVCbetFactor = Some 50m }
  let actual = toTurnOptions Face.Eight false eo
  let expected = { Options.CbetFactor = None; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFDCbetFactor = Some 70m }
  let actual = toTurnOptions Face.Eight true eo
  let expected = { Options.CbetFactor = Some 70m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet not FD card`` () =
  let eo = { defaultOptions with TurnFDCbetCards = "8,9,T"; TurnFVCbetFactor = Some 70m }
  let actual = toTurnOptions Face.Jack true eo
  let expected = { Options.CbetFactor = None; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined }
  Assert.Equal(expected, actual)
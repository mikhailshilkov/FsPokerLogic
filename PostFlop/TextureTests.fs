module TextureTests

open Xunit
open Cards.HandValues
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
  TurnFBCbetFactor = Never}

[<Fact>]
let ``toFlopOptions when no FD`` () =
  let eo = { defaultOptions with CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  let actual = toFlopOptions false false eo
  let expected = { Options.CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when no FD but board has FD and Cbet is 75m => Cbet becomes 100m`` () =
  let eo = { defaultOptions with CbetFactor = Always 75m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallRaisePet }
  let actual = toFlopOptions false true eo
  let expected = { Options.CbetFactor = Always 100m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallRaisePet; DonkRaise = OnDonkRaise.AllIn }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD`` () =
  let eo = { defaultOptions with CbetFactor = Always 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff; DonkRaise = OnDonkRaise.StackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD Cbets 50m instead of None and Stacks Off`` () =
  let eo = { defaultOptions with CbetFactor = Never; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff; DonkRaise = OnDonkRaise.StackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD but Donk FD is not defined`` () =
  let eo = { defaultOptions with CbetFactor = Never; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = None }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.CallEQ 17; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

let toTurnOptionsAdapter face isFlush p1 p2 p3 p4 =
  let board = parseBoard ("2h2d2c" + (faceToChar face |> string) + "s")
  let value = { Made = (if isFlush then Flush(Nut) else Nothing); FD = NoFD; FD2 = NoFD; SD = NoSD }
  let (r, _) = toTurnOptions board value p1 p2 p3 p4
  r

[<Fact>]
let ``toTurnOptions Cbet FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Always 62.5m }
  let actual = toTurnOptionsAdapter Face.Three false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FV card on flushy board`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Always 50m }
  let actual = toTurnOptionsAdapter Face.Three false OnDonk.Undefined OnDonkRaise.Undefined 2 eo
  let expected = { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions no Cbet not FV card`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "2,3,4"; TurnFVCbetFactor = Always 62.5m }
  let actual = toTurnOptionsAdapter Face.Five false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Cbet FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = Always 50m }
  let actual = toTurnOptionsAdapter Face.Seven false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions no Cbet not FB card`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "5,6,7"; TurnFBCbetFactor = Always 50m }
  let actual = toTurnOptionsAdapter Face.Eight false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FV Check Raise`` () =
  let eo = { defaultOptions with TurnFVCbetCards = "J"; TurnFVCbetFactor = Always 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptionsAdapter Face.Jack false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions FB Check Raise`` () =
  let eo = { defaultOptions with TurnFBCbetCards = "Q"; TurnFBCbetFactor = Always 50m; TurnCheckRaise = OnCheckRaise.StackOff }
  let actual = toTurnOptionsAdapter Face.Queen false OnDonk.Undefined OnDonkRaise.Undefined 1 eo
  let expected = { Options.CbetFactor = Always 50m; CheckRaise = OnCheckRaise.Fold; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toTurnOptions Flush`` () =
  let actual = toTurnOptionsAdapter Face.Two true OnDonk.Undefined OnDonkRaise.Undefined 1 defaultOptions
  let expected = { Options.CbetFactor = Always 62.5m; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Undefined; DonkRaise = OnDonkRaise.Undefined }
  Assert.Equal(expected, actual)
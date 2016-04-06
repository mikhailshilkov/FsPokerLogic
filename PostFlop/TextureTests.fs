module TextureTests

open Xunit
open PostFlop.Options
open PostFlop.Import
open PostFlop.Texture

[<Fact>]
let ``toFlopOptions when no FD`` () =
  let eo = { CbetFactor = Some 75; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions false false eo
  let expected = { Options.CbetFactor = Some 75; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when no FD but board has FD and Cbet is 75 => Cbet becomes 100`` () =
  let eo = { CbetFactor = Some 75; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions false true eo
  let expected = { Options.CbetFactor = Some 100; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17 }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD`` () =
  let eo = { CbetFactor = Some 50; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Some 50; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

[<Fact>]
let ``toFlopOptions when FD Cbets 50 instead of None and Stacks Off`` () =
  let eo = { CbetFactor = None; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff }
  let actual = toFlopOptions true true eo
  let expected = { Options.CbetFactor = Some 50; CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.ForValueStackOff }
  Assert.Equal(expected, actual)

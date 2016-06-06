module PreflopTests

open Hands
open Preflop
open Xunit
open FsCheck
open FsCheck.Xunit

let hudData = [
  { VillainName = "V1"; OpenRaise20_25 = 25;  OpenRaise16_19 = 19; OpenRaise14_15 = 15; LimpFold = 10 }
  { VillainName = "V2"; OpenRaise20_25 = 20;  OpenRaise16_19 = 16; OpenRaise14_15 = 14; LimpFold = 5 }
  { VillainName = "LongUnrecognizable"; OpenRaise20_25 = 30;  OpenRaise16_19 = 26; OpenRaise14_15 = 24; LimpFold = 15 }
]

[<Theory>]
[<InlineData("V2", 1)>]
[<InlineData("??ngUnrecognizabl?", 2)>]
[<InlineData("NonExisting", 0)>]
let ``hud returns a proper villain stats based on name`` name index =
  let villain = hud hudData name
  Assert.Equal(hudData.[index], villain)

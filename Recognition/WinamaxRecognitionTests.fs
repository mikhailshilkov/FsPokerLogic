module WinamaxRecognitionTests

open Xunit
open Recognition.ScreenRecognition
open Recognition.WinamaxRecognition
open System.Drawing
open System.IO

let test actual folderName =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeScreenWinamax image
    let expected = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "").Replace("_", "")
    if expected <> "null" then
      Assert.Equal(expected, actual result)
    else
      Assert.Null(actual result)

  Directory.GetFiles(@"..\..\TestCases\Winamax\" + folderName) |> Array.iter testFile

[<Fact>]
let ``recognize bet sizes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", Option.toNullable r.HeroBet, Option.toNullable r.VillainBet)) "Bets"

[<Fact>]
let ``recognize pot size from predefined file`` () =
  test (fun r -> System.String.Format("{0}", Option.toNullable r.TotalPot)) "Pot"

[<Fact>]
let ``recognize stakes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", Option.toNullable r.HeroStack, Option.toNullable r.VillainStack)) "Stakes"

[<Fact>]
let ``recognize board from predefined file`` () =
  test (fun r -> r.Board) "Flop"

[<Fact>]
let ``recognize hand from predefined file`` () =
  test (fun r -> r.HeroHand) "Hand"

[<Fact>]
let ``recognize blinds from predefined file`` () =
  test (fun r -> sprintf "%A-%A" r.Blinds.Value.SB r.Blinds.Value.BB) "Blinds"

[<Fact>]
let ``recognize dealer (button) from predefined file`` () =
  test (fun r -> match r.Button with | Hero -> "H" | Villain -> "V" | _ -> "U") "Dealer"

[<Fact>]
let ``recognize actions from predefined file`` () =
  let formatActions a =
    let names = a |> Array.map (fun x -> x.Name)
    System.String.Join("-", names)
  test (fun r -> r.Actions |> formatActions) "ActionsButtons"
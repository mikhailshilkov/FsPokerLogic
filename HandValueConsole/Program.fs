open System
open Excel.Import
open Hands
open Cards.Actions
open Cards.HandValues
open PostFlop.Options
open PostFlop.Decision
open PostFlop.Import
open PostFlop.Texture
open PostFlop.HandValue

[<EntryPoint>]
let main argv =   

  Console.Write "Opening excel files..."
  let fileNameTurnDonk = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
  let xl = useExcel fileNameTurnDonk
  Console.Write "\n"

  let mutable k = 'a'
  while k <> 'q' do
    Console.Write "\nPlease enter your hand (e.g. AsAc, 8d7h, Kh2h): "
    let handString = Console.ReadLine()
    let hand = parseFullHand handString
    let suitedHand = parseSuitedHand handString

    printf "\nPlease enter turn (e.g. 9s8c7d6h): "
    let boardString = Console.ReadLine()
    let board = parseBoard boardString
    let value = handValueWithDraws suitedHand board
    printfn "Hand value is: %A" value

    let texture = boardTexture board
    printfn "Special conditions: %A" texture

    k <- Console.ReadKey().KeyChar
  0 // return an integer exit code
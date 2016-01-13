open System
open Microsoft.Office.Interop

open Hands
open Ranges
open Preflop


[<EntryPoint>]
let main argv =   
  Console.Write "Importing excel files..."
  let xlApp = new Excel.ApplicationClass()
  let xlWorkBook = xlApp.Workbooks.Open(System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx")
  let xlWorkSheet = xlWorkBook.Worksheets.["12BB"] :?> Excel.Worksheet
  let firstValue = xlWorkSheet.Cells.Range("C1", "C1").Value2 :?> string

  printfn "%A" firstValue

  Console.Write "Please enter the hero stack (1-999): "
  let heroStack = Console.ReadLine() |> int
  let villainStack = 1000 - heroStack

  Console.Write "Please enter the big blind (20-400): "
  let bb = Console.ReadLine() |> int
  let sb = bb / 2

  let effectiveStack = (min heroStack villainStack) / bb

  Console.Write "Is Hero IP or OOP (IP/OOP): "
  let oop = Console.ReadLine() = "OOP"

  printfn "Hero – %A chips, Villain – %A chips. Blind level %A/%A. Hero is %sP\n" heroStack villainStack sb bb (if oop then "OO" else "I")

  let villainAction = 
    if oop
    then 
      Console.Write "What was the Villain action (limp, raise xx): "
      Console.ReadLine() |> Some
    else
      None

  let printAction action =
    match action with
      | Some(r) -> printfn "%A" r
      | None -> Console.WriteLine "Could not make a decision"

  while true do
    Console.Write "\nPlease enter your hand value (e.g. AA, 87s, K2o): "
    try
      let handString = Console.ReadLine()
      let parsed = parseHand handString
      let result = decide effectiveStack [] parsed

      printAction result

      match result with
      | Some(Call) -> 
        let minRaise = decide effectiveStack [Limp; Raise(2, 2)] parsed
        Console.Write "On a raise <= 60: " 
        printAction minRaise
        let bigRaise = decide effectiveStack [Limp; Raise(4, 4)] parsed
        Console.Write "On a raise > 60: " 
        printAction bigRaise
      | Some(MinRaise) ->
        let minRaise = decide effectiveStack [Raise(2, 2); Raise(2, 2)] parsed
        Console.Write "On a raise 60-67: " 
        printAction minRaise
        let raise4x = decide effectiveStack [Raise(4, 4); Raise(4, 4)] parsed
        Console.Write "On a raise 68-101: " 
        printAction raise4x
        printfn "On a raise 68-101: %A" raise4x
        let raise6x = decideRaiseRaise6x parsed
        printfn "On a raise 102-131: %A" raise6x
        let raise8x = decideRaiseRaise8x parsed
        printfn "On a raise 132+: %A" raise8x
        let allIn = decideRaiseAllIn parsed
        printfn "On all-in: %A" allIn
      | _ -> ()
    with
      | _ -> Console.WriteLine "Not a valid hand! Please enter a hand like 88, 98s or JTo..."    
  0 // return an integer exit code

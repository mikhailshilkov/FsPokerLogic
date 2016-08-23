open System

open Hands
open Ranges
open Preflop
open Import
open Excel.Import

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let (|RaiseInt|_|) (str : string) =
  let parts = str.Split(' ')
  if parts.Length = 2 && parts.[0] = "raise"
  then match System.Int32.TryParse(parts.[1]) with
       | (true,int) -> int |> decimal |> Some
       | _ -> None
  else None

let rec enterNumber text min max =
  printf "%s (%A-%A): " text min max  
  let input = Console.ReadLine()
  match input with
  | Int i when i >= min && i <= max -> i
  | _ ->
    Console.WriteLine "Not a valid number!"
    enterNumber text min max

let rec enterVillainAction bb stack =
  Console.Write "What was the Villain action (limp, raise xx): "
  let actionString = Console.ReadLine()
  match actionString.ToLowerInvariant() with
  | "limp" -> WasLimp
  | RaiseInt i when i >= stack -> WasRaiseAllIn
  | RaiseInt i -> WasRaise(i / bb)
  | _ -> 
    Console.WriteLine "Not a valid action!"
    enterVillainAction bb stack

let rec enterPosition () =
  Console.Write "Is Hero IP or OOP (IP/OOP): "
  let input = Console.ReadLine()
  match input.ToUpperInvariant() with
  | "IP" -> IP
  | "OOP" -> OOP
  | _ ->
    Console.WriteLine "Not a valid position!"
    enterPosition()

[<EntryPoint>]
let main argv =   
  Console.Write "Importing excel files..."
  let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
  let rulesIP = importExcel (importRulesByStack importRulesIP) fileNameIP |> List.ofSeq
  let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
  let rulesOOP = importExcel (importRulesByStack importRulesOOP) fileNameOOP |> List.ofSeq
  let rules = List.concat [|rulesIP;rulesOOP|]
  let decide = decideOnRules rules
  Console.Write " done!\n\n"

  while true do
    let heroStack = enterNumber "Please enter the hero stack" 1 999
    let villainStack = 1000 - heroStack

    let bb = enterNumber "Please enter the big blind" 20 500
    let sb = bb / 2
    let stack = min heroStack villainStack
    let effectiveStack = (decimal stack) / (decimal bb)

    let position = enterPosition()

    printfn "Hero – %A chips, Villain – %A chips. Blind level %A/%A. Hero is %A\n" heroStack villainStack sb bb position

    let printAction action =
      match action with
        | Some(r) -> printfn "%A" r
        | None -> Console.WriteLine "Could not make a decision"

    let mutable sameHand = true
    while sameHand do
      Console.Write "\nPlease enter your hand value (e.g. AA, 87s, K2o) or space to change stacks: "
      try
        let handString = Console.ReadLine()
        if not (String.IsNullOrWhiteSpace handString) then
          let parsed = parseHand handString

          let previous = if position = OOP then [enterVillainAction (decimal bb) (decimal stack)] else []
          let result = decide effectiveStack 0 0m previous parsed

          printAction result

          match result with
          | Some(Call) -> 

            let raiseSize = enterNumber "Villain raises. What's the raise size" (bb * 2) stack 
            let x = (raiseSize / bb) |> decimal
            let onRaise = decide effectiveStack 0 0m [WasLimp; WasRaise(x)] parsed
            printAction onRaise

          | Some(MinRaise) ->
            let raiseSize = enterNumber "Villain raises. What's the raise size" (bb * 3) stack 
            let x = (raiseSize |> decimal) / (bb |> decimal)
            let allActions = if raiseSize = stack then List.append previous [WasRaise(2m); WasRaiseAllIn] else List.append previous [WasRaise(2m); WasRaise(x)]
            let onRaise = decide effectiveStack 0 0m allActions parsed
            printAction onRaise

          | _ -> ()
        else 
          Console.Write "\n\n------------------\nNew Hand\n------------------\n"
          sameHand <- false
      with
        | a -> 
          Console.WriteLine a
          Console.WriteLine "Not a valid input! Please enter a hand like 88, 98s or JTo..."    
  0 // return an integer exit code

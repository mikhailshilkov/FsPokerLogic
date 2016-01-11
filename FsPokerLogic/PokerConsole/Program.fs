open Hands
open Ranges
open Preflop
open System

[<EntryPoint>]
let main argv =   
  Console.Write "Player A – 500 chips, Player B – 500 chips. Blind level 10/20. Hero is IP\n"
  while true do
    Console.Write "\nPlease enter your hard value (e.g. AA, 87s, K2o): "
    try
      let handString = Console.ReadLine()
      let parsed = parseHand handString
      let result = decide parsed
      printfn "%A" result

      match result with
      | Call -> 
        let minRaise = decideLimpMinRaise parsed
        printfn "On a raise <= 60: %A" minRaise
        let bigRaise = decideLimpBigRaise parsed
        printfn "On a raise > 60: %A" bigRaise
      | MinRaise ->
        let minRaise = decideRaiseMinRaise parsed
        printfn "On a raise 60-67: %A" minRaise
        let raise4x = decideRaiseRaise4x parsed
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

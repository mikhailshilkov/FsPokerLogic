﻿namespace Recognition

module WinamaxRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open Microsoft.FSharp.Core
  open StringRecognition
  open HandRecognition
  open Recognition.ScreenRecognition

  let recognizeScreenWinamax (bitmap : Bitmap) (title: string) =
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let chooseGoodNumber minLength (ss : string list) =
      ss 
      |> List.filter (fun (s : string) -> s <> null && s.Length >= minLength && not(s.Contains("?")))
      |> List.choose parseNumber
      |> List.tryHead

    let isAllIn x = 
      let isRed (c : Color) = c.B < 50uy && c.G < 50uy && c.R > 100uy
      hasSpecialColor isRed x

    let getTitleBlinds (title: string) =
      let parts = title.Split([|" / "; " NL "|], StringSplitOptions.RemoveEmptyEntries)
      let blinds = if parts.Length > 2 then Some(parts.[1]) else None
      blinds |> Option.bind (parseBlinds '-')

    let chooseBlinds titleBlinds cornerBlinds (hasFlop, heroBet, villainBet) =
      match titleBlinds, cornerBlinds, heroBet, villainBet with
      | Some tb, Some cb, Some hb, Some vb -> 
        if (not hasFlop) && (hb = cb.BB || vb = cb.BB) then cornerBlinds
        else titleBlinds
      | Some tb, None, _, _ -> titleBlinds
      | _ -> cornerBlinds
    
    let totalPot = 
      chooseGoodNumber 2 [
        recognizeWinamaxWhiteNumber (getPixel 341 276) 20 13
        recognizeWinamaxWhiteNumber (getPixel 353 276) 20 13
        recognizeWinamaxPotNumber (getPixel 334 274) 40 14] 
    
    let heroBet = recognizeWinamaxNumber (getPixel 310 315) 30 13 |> parseNumber
    let heroStack = recognizeWinamaxStackNumber (getPixel 310 381) 30 15 |> parseNumber

    let villainBet = recognizeWinamaxNumber (getPixel 310 159) 30 13 |> parseNumber
    let villainStackSitout = recognizeWinamaxStackSitoutNumber (getPixel 310 109) 30 15 |> parseNumber
    let villainStack = 
      if isAllIn (getPixel 306 111) 38 11 then Some 0
      else Option.choose villainStackSitout (recognizeWinamaxStackNumber (getPixel 310 109) 30 15 |> parseNumber)

    let button = 
      if isYellowButton (getPixel 255 324) 16 16 then Hero
      else if isYellowButton (getPixel 379 127) 16 16 then Villain else Unknown

    let a = parsePattern (getPixel 299 332) 9 15

    let hasHand = isFlop (getPixel 325 330) 25 15
    let heroHand = 
      if hasHand then
        (recognizeCard winamaxPatterns (getPixel 298 332) 9 15) + (recognizeCard winamaxPatterns (getPixel 314 332) 9 15)
      else ""

    let hasFlop = isFlop (getPixel 197 189) 150 70
    let flop = 
      if hasFlop then
        [197; 251; 305; 360; 413]
        |> Seq.map (fun x -> recognizeCard winamaxPatterns (getPixel x 189) 9 15)
        |> String.concat ""
      else null

    let blinds = 
      chooseBlinds
        (getTitleBlinds title) 
        (recognizeWinamaxBlinds (getPixel 462 28) 45 14  |> parseBlinds '-')
        (hasFlop, heroBet, villainBet)

    let actions = 
      [("Fold", winamaxFold, 194, 424, 60, 15)
       ("Check", winamaxCheck, 300, 424, 45, 15)
       ("Call", winamaxCall, 305, 434, 35, 9)
       ("Bet", winamaxBet, 410, 434, 30, 9)
       ("RaiseTo", winamaxRaiseTo, 405, 434, 40, 9)]
      |> Seq.map (fun (n, p, x, y, w, h) -> (recognizeBlock p (getPixel x y) w h), n, (x, y, w, h))
      |> Seq.filter (fun (m, _, _) -> m)
      |> Seq.map (fun (_, n, r) -> { Name = n; Region = r })
      |> Array.ofSeq      
      |> Array.append ([|{ Name = "Max"; Region = (350, 469, 35, 8) }|])

    { Room = Winamax
      TotalPot = totalPot
      HeroStack = heroStack
      VillainStack = villainStack
      HeroBet = heroBet
      VillainBet = villainBet
      VillainName = "???"
      HeroHand = if heroHand = "" then null else heroHand
      Button = button
      Actions = actions
      Blinds = blinds
      Board = flop
      Sitout = if villainStackSitout.IsSome then Seat.Villain else Seat.Unknown
      AmountInput = (490, 451, 60, 9)
    }

  let recognizeBetSizeWinamax (bitmap : Bitmap) =    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    recognizeWinamaxBetSize (getPixel 445 450) 26 12 |> parseNumber
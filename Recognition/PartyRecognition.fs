namespace Recognition

module PartyRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open Microsoft.FSharp.Core
  open StringRecognition
  open HandRecognition
  open Recognition.ScreenRecognition

  let recognizeScreenParty (bitmap : Bitmap) (title: string) =
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let totalPot = recognizePartyPotNumber (getPixel 380 58) 55 14  |> parseNumber
    
    let heroStack = recognizePartyStackNumber (getPixel 131 382) 45 14 |> parseNumber
    let heroBet = recognizePartyBetNumber (getPixel 170 292) 50 12 |> parseNumber

    let isVillainAllin =
      let isYellow' dx dy = isYellow (getPixel 629 383 dx dy) = StringRecognition.W
      let leftParenPixels = countIn isYellow' 0 0 2 13
      let rightParenPixels = countIn isYellow' 52 0 54 13
      leftParenPixels >= 10 && rightParenPixels >= 10
    let villainStack = 
      if isVillainAllin then Some 0
      else recognizePartyStackNumber (getPixel 638 382) 45 14 |> parseNumber
    let villainBet = recognizePartyBetNumber (getPixel 590 292) 50 12 |> parseNumber

    let hasHand = isFlop (getPixel 632 317) 25 15
    let heroHand = 
      if hasHand then
        (recognizeCard partyPatterns (getPixel (631+5) (316+5)) 11 17) + (recognizeCard partyPatterns (getPixel (654+5) (316+5)) 11 17)
      else ""

    let hasFlop = isFlop (getPixel 248 189) 150 70
    let flop = 
      if hasFlop then
        [248; 313; 378; 443; 509]
        |> Seq.map (fun x -> recognizeCard partyPatterns (getPixel (x+5) (189+5)) 11 17)
        |> String.concat ""
      else null

    let button = 
      let isWhite' x y dx dy = isWhite (getPixel x y dx dy) = StringRecognition.W
      let isDealer x = (countIn (isWhite' x 314) 0 0 20 16) >= 100
      if isDealer 220 then Hero
      else if isDealer 571 then Villain else Unknown

    let s = parseStringPattern (getPixel 59 47) isPaleWhite 6 8 
    //let a = parsePattern (getPixel 41 47) 6 8

    let blinds = recognizePartyBlinds (getPixel 41 46) 45 10  |> parseBlinds '/'

    let actions = 
      [("Fold", partyFold, 430, 460, 515, 50)
       ("Check", partyCheck, 558, 580, 515, 70)
       ("Call", partyCall, 558, 595, 506, 40)
       ("AllIn", partyAllIn, 558, 585, 506, 55)
       ("Bet", partyBet, 690, 725, 507, 40)
       ("RaiseTo", partyRaiseTo, 690, 705, 507, 80)]
      |> Seq.map (fun (n, p, bx, x, y, w) -> (recognizeBlock p (getPixel x y) w 20), n, bx)
      |> Seq.filter (fun (m, _, _) -> m)
      |> Seq.map (fun (_, n, x) -> { Name = n; Region = (x, 510, 115, 35) })
      |> Array.ofSeq      
      |> Array.append ([|{ Name = "Max"; Region = (660, 444, 48, 19) }|])

    { Room = Party
      TotalPot = totalPot
      HeroStack = heroStack
      VillainStack = villainStack
      HeroBet = heroBet
      VillainBet = villainBet
      VillainName = "???"
      HeroHand = heroHand
      Button = button
      Actions = actions
      Blinds = blinds
      Board = flop
      Sitout = Unknown
      AmountInput = (730, 460, 60, 17)
    }

  let recognizeBetSizeParty (bitmap : Bitmap) =    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let s = parseStringPattern (getPixel 753 463) isBlack 8 11 
    recognizePartyBetSize (getPixel 740 460) 50 17 |> parseNumber
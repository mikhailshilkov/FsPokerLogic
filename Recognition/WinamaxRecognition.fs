namespace Recognition

module WinamaxRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open StringRecognition
  open HandRecognition
  open Recognition.ScreenRecognition

  let recognizeScreenWinamax (bitmap : Bitmap) =
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = 
      if not(String.IsNullOrEmpty s) then
        let (s, r) = Int32.TryParse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture)
        if s then Some r else None
      else None

    let parseBlinds (s : string) =
      if not(String.IsNullOrEmpty s) then
        try
          let parts = s.Split('-')
          let sb = parseNumber parts.[0]
          let bb = if parts.Length > 1 then parseNumber parts.[1] else None
          Option.bind (fun v -> Option.map (fun vb -> { SB = v; BB = vb }) bb ) sb
        with
          | e -> None
      else None

    let chooseGoodNumber minLength (ss : string list) =
      ss 
      |> List.filter (fun (s : string) -> s <> null && s.Length >= minLength && not(s.Contains("?")))
      |> List.choose parseNumber
      |> List.tryHead
    
    let p11111 = parseStringPattern (getPixel 490 31) 5 9
    let p2 = parsePattern (getPixel 330 382) 6 13

    let blinds = recognizeWinamaxBlinds (getPixel 462 28) 45 14  |> parseBlinds
    let totalPot = 
      chooseGoodNumber 2 [
        recognizeWinamaxWhiteNumber (getPixel 341 276) 20 13
        recognizeWinamaxWhiteNumber (getPixel 353 276) 20 13
        recognizeWinamaxPotNumber (getPixel 334 274) 40 14] 
    
    let heroBet = recognizeWinamaxNumber (getPixel 310 315) 30 13 |> parseNumber
    let heroStack = recognizeWinamaxBetNumber (getPixel 310 381) 30 15 |> parseNumber

    let villainBet = recognizeWinamaxNumber (getPixel 310 159) 30 13 |> parseNumber
    let villainStack = recognizeWinamaxBetNumber (getPixel 310 109) 30 15 |> parseNumber

    let button = 
      if isYellowButton (getPixel 255 324) 16 16 then Hero
      else if isYellowButton (getPixel 379 127) 16 16 then Villain else Unknown

    let heroHand = 
        (recognizeCard winamaxPatterns (getPixel 299 332) 9 15) + (recognizeCard winamaxPatterns (getPixel 315 332) 9 15)

    let flop =
      [197; 251; 305; 360; 413]
      |> Seq.map (fun x -> recognizeCard winamaxPatterns (getPixel x 189) 9 15)
      |> String.concat ""

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

    { TotalPot = totalPot
      HeroStack = heroStack
      VillainStack = villainStack
      HeroBet = heroBet
      VillainBet = villainBet
      VillainName = null
      HeroHand = if heroHand = "" then null else heroHand
      Button = button
      Actions = actions
      Blinds = blinds
      Board = flop
      Sitout = Seat.Unknown
    }
namespace Recognition

module ScreenRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open StringRecognition
  open HandRecognition

  type Seat = Hero | Villain | Unknown

  type Blinds = {
    SB: int
    BB: int
  }

  type ActionButton = {
    Name: string
    Region: (int * int * int * int)
  }

  type Screen = {
    TotalPot: int option
    HeroStack: int option
    VillainStack: int option
    HeroBet: int option
    VillainBet: int option
    VillainName: string
    HeroHand: string
    Actions: ActionButton[]
    Blinds: Blinds option
    Button: Seat
    Board: string
    Sitout: Seat
  }

  let print screen =
    [sprintf "Villain: %s %s" screen.VillainName (if screen.Sitout = Villain then "(sitout)" else "");
     sprintf "Total pot: %A" (Option.toNullable screen.TotalPot);
     sprintf "Blinds: %A" screen.Blinds;
     sprintf "Stacks: %A/%A" (Option.toNullable screen.HeroStack) (Option.toNullable screen.VillainStack);
     sprintf "Bets: %A/%A" (Option.toNullable screen.HeroBet) (Option.toNullable screen.VillainBet);
     sprintf "Hand: %s (%s)" screen.HeroHand (match screen.Button with | Hero -> "IP" | Villain -> "OOP" | Unknown -> "?");
     sprintf "Board: %s" screen.Board;
     sprintf "Actions: %A" screen.Actions]

  let recognizeScreen (bitmap : Bitmap) =
    
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
          let parts = s.Split('/')
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

    let blinds = recognizeBlinds (getPixel 308 7) 70 16  |> parseBlinds
    let totalPot = 
      chooseGoodNumber 2 [recognizeNumber (getPixel 302 133) 35 15; recognizeNumber (getPixel 302 77) 35 15; recognizeNumber (getPixel 278 93) 50 15] 
    let heroStack = recognizeNumber (getPixel 100 342) 50 14 |> parseNumber
    let villainStack = recognizeNumber (getPixel 500 342) 50 14 |> parseNumber
    let heroBet = 
      chooseGoodNumber 2 [recognizeNumber (getPixel 82 245) 50 15; recognizeNumber (getPixel 82 231) 50 15] 
    let villainBet = 
      chooseGoodNumber 2 [recognizeNumber (getPixel 462 301) 50 15; recognizeNumber (getPixel 517 231) 50 15] 
    let villainName = recognizeText (getPixel 493) 327 90 10
    let mask = (parseStringPattern (getPixel 513 328) 4 10)
    
    let actions = 
      [(360, 433, 70, 20); (450, 427, 70, 17); (450, 433, 70, 20); (540, 427, 70, 17)]
      |> Seq.map (fun (x, y, w, h) -> (recognizeButton (getPixel x y) w h), (x, y, w, h))
      |> Seq.filter (fun (x, _) -> not (String.IsNullOrEmpty x) && not(x.Contains("?")))
      |> Seq.map (fun (x, r) -> { Name = x; Region = r })
      |> Array.ofSeq      

    let button = 
      if isGreenButton (getPixel 159 314) 17 17 then Hero
      else if isGreenButton (getPixel 476 326) 17 17 then Villain else Unknown

    let hasFlop = isFlop (getPixel 212 178) 131 60

    let isVillainSitout = isVillainSitout (getPixel 570 319) 12 11
    let isHeroSitout = isHeroSitout (getPixel 489 469) 11 11
    let actionsWithCheckboxes = actions |> Array.append (if isHeroSitout then [|{ Name = "SitBack"; Region = (492, 472, 7, 5) }|] else [||])

    let (dxo, dyo) = findCardStart (getPixel 78 274) 12 17
    let heroHand = 
      match (dxo, dyo, isHeroSitout) with 
      | Some dx, Some dy, false ->
        (recognizeCard ipokerPatterns (getPixel (79+dx) (274+dy)) 12 17) + (recognizeCard ipokerPatterns (getPixel (116+dx) (274+dy)) 12 17)
      | _, _, _ -> null

    let (dxo, dyo) = findCardStart (getPixel 223 185) 12 17
    let flop = 
      match (dxo, dyo) with 
      | Some dx, Some dy ->
        [223; 261; 298; 335; 373]
        |> Seq.map (fun x -> recognizeCard ipokerPatterns (getPixel (x+dx) (185+dy)) 12 17)
        |> String.concat ""
      | _, _ -> null

    { TotalPot = totalPot
      HeroStack = heroStack
      VillainStack = villainStack
      HeroBet = heroBet
      VillainBet = villainBet
      VillainName = villainName
      HeroHand = if heroHand = "" then null else heroHand
      Button = button
      Actions = actionsWithCheckboxes
      Blinds = blinds
      Board = flop
      Sitout = if isHeroSitout then Hero else if isVillainSitout then Villain else Unknown }

  let recognizeBetSize (bitmap : Bitmap) =    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let (dxo, dyo) = findCardStart (getPixel 575 403) 8 8
    match (dxo, dyo) with 
    | Some dx, Some dy ->
      recognizeBetSize (getPixel (574 + dx) (402 + dy)) 40 14
    | _ -> null

  let isHeroSitout (bitmap : Bitmap) =    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    isHeroSitout (getPixel 489 469) 11 11

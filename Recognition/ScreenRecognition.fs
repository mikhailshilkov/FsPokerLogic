namespace Recognition


module ScreenRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open StringRecognition
  open HandRecognition

  type ButtonPosition = Hero | Villain | Unknown

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
    HeroHand: string
    Actions: ActionButton[]
    Blinds: Blinds option
    Button: ButtonPosition
    Flop: string
    IsVillainSitout: bool
  }

  let print screen =
    [sprintf "Total pot: %A" (Option.toNullable screen.TotalPot);
     sprintf "Blinds: %A" screen.Blinds;
     sprintf "Stacks: %A/%A" (Option.toNullable screen.HeroStack) (Option.toNullable screen.VillainStack);
     sprintf "Bets: %A/%A" (Option.toNullable screen.HeroBet) (Option.toNullable screen.VillainBet);
     sprintf "Hand: %s (%s)" screen.HeroHand (match screen.Button with | Hero -> "IP" | Villain -> "OOP" | Unknown -> "?");
     sprintf "Actions: %A" screen.Actions]

  let recognizeScreen (bitmap : Bitmap) =
    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = 
      try
        Int32.Parse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture) |> Some
      with
        | e -> None

    let parseBlinds (s : string) =
      try
        let parts = s.Split('/')
        let sb = parseNumber parts.[0]
        let bb = parseNumber parts.[1]
        Option.bind (fun v -> Option.map (fun vb -> { SB = v; BB = vb }) bb ) sb
      with
        | e -> None

    let chooseGoodNumber minLength (ss : string list) =
      ss 
      |> List.filter (fun (s : string) -> s <> null && s.Length >= minLength && not(s.Contains("?")))
      |> List.map parseNumber
      |> List.filter Option.isSome
      |> List.map Option.get
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
    
    let actions = 
      [(360, 433, 70, 20); (450, 427, 70, 17); (450, 433, 70, 20); (540, 427, 70, 17)]
      |> Seq.map (fun (x, y, w, h) -> (recognizeButton (getPixel x y) w h), (x, y, w, h))
      |> Seq.filter (fun (x, _) -> not (String.IsNullOrEmpty x) && not(x.Contains("?")))
      |> Seq.map (fun (x, r) -> { Name = x; Region = r })
      |> Array.ofSeq      

    let button = 
      if isButton (getPixel 159 314) 17 17 then Hero
      else if isButton (getPixel 476 326) 17 17 then Villain else Unknown

    let hasFlop = isFlop (getPixel 212 178) 131 60

    let isVillainSitout = isVillainSitout (getPixel 570 319) 12 11

    let (dxo, dyo) = findCardStart (getPixel 78 274) 13 17
    let heroHand = 
      match (dxo, dyo) with 
      | Some dx, Some dy ->
        (recognizeCard (getPixel (78+dx) (274+dy)) 13 17) + (recognizeCard (getPixel (115+dx) (274+dy)) 13 17)
      | _, _ -> null

    let (dxo, dyo) = findCardStart (getPixel 223 185) 13 17
    let flop = 
      match (dxo, dyo) with 
      | Some dx, Some dy ->
        [222; 260; 297; 334; 372]
        |> Seq.map (fun x -> recognizeCard (getPixel (x+dx) (185+dy)) 13 17)
        |> String.concat ""
      | _, _ -> null

    { TotalPot = totalPot
      HeroStack = heroStack
      VillainStack = villainStack
      HeroBet = heroBet
      VillainBet = villainBet
      HeroHand = if heroHand = "" then null else heroHand
      Button = button
      Actions = actions
      Blinds = blinds
      Flop = flop
      IsVillainSitout = isVillainSitout }

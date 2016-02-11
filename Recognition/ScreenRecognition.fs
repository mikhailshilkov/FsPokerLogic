namespace Recognition


module ScreenRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open StringRecognition
  open HandRecognition

  type ButtonPosition = Hero | Villain | Unknown

  type Screen = {
    TotalPot: int option
    HeroStack: int option
    VillainStack: int option
    HeroBet: int option
    VillainBet: int option
    HeroHand: string
    Actions: string
    Blinds: string
    Button: ButtonPosition
  }

  let print screen =
    [sprintf "Total pot: %A" (Option.toNullable screen.TotalPot);
     sprintf "Blinds: %s" screen.Blinds;
     sprintf "Stacks: %A/%A" (Option.toNullable screen.HeroStack) (Option.toNullable screen.VillainStack);
     sprintf "Bets: %A/%A" (Option.toNullable screen.HeroBet) (Option.toNullable screen.VillainBet);
     sprintf "Hand: %s (%s)" screen.HeroHand (match screen.Button with | Hero -> "IP" | Villain -> "OOP" | Unknown -> "?");
     sprintf "Actions: %s" screen.Actions]

  let recognizeScreen (bitmap : Bitmap) =
    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = 
      try
        Int32.Parse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture) |> Some
      with
        | e -> None

    let chooseGoodString minLength (s1 : string) (s2 : string) =
      if s1 <> null && s1.Length >= minLength && not(s1.Contains("?")) then s1
      else s2

    let blinds = recognizeBlinds (getPixel 308 7) 70 16    
    let totalPot = 
      chooseGoodString 2 (recognizeNumber (getPixel 302 133) 35 15) (recognizeNumber (getPixel 302 77) 35 15) |> parseNumber
    let heroStack = recognizeNumber (getPixel 100 342) 50 14 |> parseNumber
    let villainStack = recognizeNumber (getPixel 500 342) 50 14 |> parseNumber
    let heroBet = recognizeNumber (getPixel 82 245) 50 15 |> parseNumber
    let villainBet = recognizeNumber (getPixel 462 301) 50 15 |> parseNumber
    
    let actions = 
      [recognizeButton (getPixel 360 433) 70 20;
       chooseGoodString 3 (recognizeButton (getPixel 450 427) 70 17) (recognizeButton (getPixel 450 433) 70 20);
       recognizeButton (getPixel 540 427) 70 17]
      |> List.filter (fun x -> not (String.IsNullOrEmpty x))
      |> String.concat "|"

    let button = 
      if isButton (getPixel 159 314) 17 17 then Hero
      else if isButton (getPixel 476 326) 17 17 then Villain else Unknown

    let (dxo, dyo) = findCardStart (getPixel 78 274) 13 17
    let heroHand = 
      match (dxo, dyo) with 
      | Some dx, Some dy ->
        (recognizeCard (getPixel (78+dx) (274+dy)) 13 17) + (recognizeCard (getPixel (115+dx) (274+dy)) 13 17)
      | _, _ -> null

    { TotalPot = totalPot; 
      HeroStack = heroStack; 
      VillainStack = villainStack; 
      HeroBet = heroBet; 
      VillainBet = villainBet; 
      HeroHand = if heroHand = "" then null else heroHand;
      Button = button;
      Actions = actions;
      Blinds = blinds }

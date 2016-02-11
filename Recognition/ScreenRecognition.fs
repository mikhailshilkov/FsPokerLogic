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
    Button: ButtonPosition
  }

  let print screen =
    [sprintf "Total pot: %A" (Option.toNullable screen.TotalPot);
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

    let chooseGoodString (s1 : string) (s2 : string) =
      if s1 <> null && s1.Length > 2 && not(s1.Contains("?")) then s1
      else s2
    
    let totalPot = recognizeNumber (getPixel 302 133) 35 15 |> parseNumber
    let heroStack = recognizeNumber (getPixel 100 342) 50 14 |> parseNumber
    let villainStack = recognizeNumber (getPixel 500 342) 50 14 |> parseNumber
    let heroBet = recognizeNumber (getPixel 82 245) 50 15 |> parseNumber
    let villainBet = recognizeNumber (getPixel 462 301) 50 15 |> parseNumber
    
    let actions = 
      [recognizeButton (getPixel 240 440) 70 18;
       chooseGoodString (recognizeButton (getPixel 340 430) 70 17) (recognizeButton (getPixel 340 440) 70 18);
       recognizeButton (getPixel 440 430) 70 17]
      |> List.filter (fun x -> not (String.IsNullOrEmpty x))
      |> String.concat "|"

    let button = 
      if isButton (getPixel 159 314) 17 17 then Hero
      else if isButton (getPixel 476 326) 17 17 then Villain else Unknown

    let heroHand = (recognizeCard (getPixel 79 276) 13 17) + (recognizeCard (getPixel 116 276) 13 17)

    { TotalPot = totalPot; 
      HeroStack = heroStack; 
      VillainStack = villainStack; 
      HeroBet = heroBet; 
      VillainBet = villainBet; 
      HeroHand = if heroHand = "" then null else heroHand;
      Button = button;
      Actions = actions }

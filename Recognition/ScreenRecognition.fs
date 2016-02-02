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

  let recognizeScreen (bitmap : Bitmap) =
    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = 
      try
        Int32.Parse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture) |> Some
      with
        | e -> None
    
    let totalPot = recognizeNumber (getPixel 302 133) 35 15 |> parseNumber
    let heroStack = recognizeNumber (getPixel 100 341) 50 15 |> parseNumber
    let villainStack = recognizeNumber (getPixel 500 341) 50 15 |> parseNumber
    let heroBet = recognizeNumber (getPixel 82 245) 50 15 |> parseNumber
    let villainBet = recognizeNumber (getPixel 462 301) 50 15 |> parseNumber
    
    let actions = 
      [recognizeButton (getPixel 240 440) 70 18;
       recognizeButton (getPixel 340 430) 70 17;
       recognizeButton (getPixel 440 430) 70 17]
      |> List.filter (fun x -> not (String.IsNullOrEmpty x))
      |> String.concat "|"

    let button = 
      if isButton (getPixel 159 313) 18 18 then Hero
      else if isButton (getPixel 482 327) 18 18 then Villain else Unknown

    let heroHand = (recognizeCard (getPixel 79 276) 13 17) + (recognizeCard (getPixel 116 276) 13 17)

    { TotalPot = totalPot; 
      HeroStack = heroStack; 
      VillainStack = villainStack; 
      HeroBet = heroBet; 
      VillainBet = villainBet; 
      HeroHand = heroHand;
      Button = button;
      Actions = actions }

namespace Recognition


module ScreenRecognition =
  open System
  open System.Drawing
  open System.Globalization
  open StringRecognition

  type Screen = {
    TotalPot: int
    HeroStack: int
    VillainStack: int
    HeroBet: int
    VillainBet: int
  }

  let recognizeScreen (bitmap : Bitmap) =
    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = 
      try
        Int32.Parse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture)
      with
        | e -> -1
    
    let totalPot = recognizeString (getPixel 302 133) 35 15 |> parseNumber
    let heroStack = recognizeString (getPixel 100 341) 50 15 |> parseNumber
    let villainStack = recognizeString (getPixel 500 341) 50 15 |> parseNumber
    let heroBet = recognizeString (getPixel 82 245) 50 15 |> parseNumber
    let villainBet = recognizeString (getPixel 462 301) 50 15 |> parseNumber

    { TotalPot = totalPot; HeroStack = heroStack; VillainStack = villainStack; HeroBet = heroBet; VillainBet = villainBet }

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
  }

  let recognizeScreen (bitmap : Bitmap) =
    
    let getPixel offsetX offsetY x y = 
      bitmap.GetPixel(offsetX + x, offsetY + y)

    let parseNumber (s : string) = Int32.Parse(s, NumberStyles.AllowThousands, CultureInfo.InvariantCulture);
    
    let totalPot = recognizeString (getPixel 302 133) 35 15 |> parseNumber
    let heroStack = recognizeString (getPixel 100 341) 50 15 |> parseNumber
    let villainStack = recognizeString (getPixel 500 341) 50 15 |> parseNumber
    { TotalPot = totalPot; HeroStack = heroStack; VillainStack = villainStack }
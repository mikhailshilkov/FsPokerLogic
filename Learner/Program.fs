open System
open System.Drawing
open Recognition.HandRecognition

[<EntryPoint>]
let main argv = 
    let image = new Bitmap(@"C:\Users\kiwo_000\Downloads\Pkr\K5.bmp");

    let getPixel offsetX offsetY x y = 
      image.GetPixel(offsetX + x, offsetY + y)

    let s = parsePattern (getPixel 79 276) 13 17

    let result = (recognizeCard (getPixel 79 276) 13 17) + (recognizeCard (getPixel 116 276) 13 17)
    let result2 = recognizeCard (getPixel 212 178) 13 17

    printfn "%s %s" result result2
    Console.ReadKey() |> ignore
    0 // return an integer exit code

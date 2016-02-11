open System
open System.Drawing
open Recognition.HandRecognition
open Recognition.StringRecognition
open Recognition.ScreenRecognition

[<EntryPoint>]
let main argv = 
    let image = new Bitmap(@"C:\Work\FsPokerLogic\Recognition\TestCases\PotStacks\30-430-540.bmp");

    let getPixel offsetX offsetY x y = 
      image.GetPixel(offsetX + x, offsetY + y)

    let result = recognizeScreen image

    //let result = recognizeBlinds (getPixel 308 7) 70 16
   // let result = parsePattern (getPixel 119 276) 13 17

//    let getPixel offsetX offsetY x y = 
//      image.GetPixel(offsetX + x, offsetY + y)
//
  //  let result = parsePattern (getPixel 336 13) 6 8
//
//    let result = (recognizeCard (getPixel 79 276) 13 17) + (recognizeCard (getPixel 116 276) 13 17)
//    let result2 = recognizeCard (getPixel 212 178) 13 17
//
    printfn "%A" result
    Console.ReadKey() |> ignore
    0 // return an integer exit code

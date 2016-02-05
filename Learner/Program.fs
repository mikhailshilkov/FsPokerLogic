open System
open System.Drawing
open Recognition.HandRecognition
open Recognition.StringRecognition
open Recognition.ScreenRecognition

[<EntryPoint>]
let main argv = 
    let image = new Bitmap(@"C:\Work\FsPokerLogic\Recognition\TestCases\null.bmp");

    let getPixel offsetX offsetY x y = 
      image.GetPixel(offsetX + x, offsetY + y)

    let result = recognizeScreen image

    //let result = recognizeButton (getPixel 0 0) 29 9
    //let result = parsePattern (getPixel 23 0) 6 9

//    let getPixel offsetX offsetY x y = 
//      image.GetPixel(offsetX + x, offsetY + y)
//
//    let s = parsePattern (getPixel 79 276) 13 17
//
//    let result = (recognizeCard (getPixel 79 276) 13 17) + (recognizeCard (getPixel 116 276) 13 17)
//    let result2 = recognizeCard (getPixel 212 178) 13 17
//
    printfn "%A" result
    Console.ReadKey() |> ignore
    0 // return an integer exit code

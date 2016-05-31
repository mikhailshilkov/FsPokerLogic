namespace Player

open System
open System.Threading
open Interaction
open Akka.FSharp
open Recognition
open Recognition.ScreenRecognition

module Click =

  type ClickTarget = (int * int * int * int)
  type ClickAction =
    | Click of ClickTarget
    | Amount of int

  type ClickerMessage = {
    WindowTitle: string
    Clicks: ClickAction[]
    IsInstant: bool
    Screen: Screen
  }

  let executeClickAction window (x, y, w, h) =
    let l = InteractionFacade.Focus(window)
    Clicker.clickRegion (l.X + x + w / 10, l.Y + y + h / 10, l.X + x + w * 9 / 10, l.Y + y + h * 9 / 10)
    Thread.Sleep(100)

  let enterAmount i =
    Clicker.backspace 3
    Clicker.enterText <| i.ToString()

  let executeAction window action =
    match action with
    | Click x -> executeClickAction window x
    | Amount x -> enterAmount x

  let r = new Random()
  let click' msg =
    let rec imp attempts =
      msg.Clicks |> Array.iter (executeAction msg.WindowTitle)
      if attempts > 1 then
        // Check that button click OK
        let w = InteractionFacade.GetWindow(msg.WindowTitle, new System.Drawing.Size(650, 490))
        let s = ScreenRecognition.recognizeScreen(w.Bitmap)
        if s = msg.Screen then imp (attempts-1)
    if not msg.IsInstant then Thread.Sleep(r.Next(200, 1000))
    imp 3
    // Move to random place below
    Thread.Sleep(150)
    Clicker.shiftBy (100, 50, 150, 100)

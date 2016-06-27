namespace Player

open System
open System.Threading
open Interaction
open Akka.FSharp
open Recognition
open Recognition.ScreenRecognition

module Click =

  type ClickAction =
    | Click of ActionButton
    | Amount of int

  type ClickerMessage = {
    WindowTitle: string
    Clicks: ClickAction[]
    IsInstant: bool
    Screen: Screen
  }

  let executeClickAction window b =
    let preconditionMet = 
      match b.Name with
      | "SitBack" -> 
        let w = InteractionFacade.GetWindow(window, new System.Drawing.Size(650, 490))
        ScreenRecognition.isHeroSitout(w.Bitmap)
      | _ -> true
    if preconditionMet then
      let (x, y, w, h) = b.Region
      let l = InteractionFacade.Focus(window)
      Clicker.clickRegion (l.X + x + w / 10, l.Y + y + h / 10, l.X + x + w * 9 / 10, l.Y + y + h * 9 / 10)
      Thread.Sleep(100)

  let enterAmount window i =
    let rec imp attempts =
      executeClickAction window { Region = (599, 407, 18, 9); Name = "AmountInput" }
      Clicker.backspace 3
      Clicker.enterText <| i.ToString()
      Thread.Sleep(100)
      if attempts > 1 then
        // Check that amount is OK
        let w = InteractionFacade.GetWindow(window, new System.Drawing.Size(650, 490))
        let b = ScreenRecognition.recognizeBetSize(w.Bitmap)
        if b <> i.ToString() then 
          imp (attempts-1)
    imp 3

  let executeAction window action =
    match action with
    | Click x -> executeClickAction window x
    | Amount x -> enterAmount window x

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

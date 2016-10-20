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

  let ensureAmount window screen check repeat =
    // Check that amount is OK
    let w = InteractionFacade.GetWindow(window, new System.Drawing.Size(650, 490))
    let b = ScreenRecognition.recognizeBetSize(w.Bitmap)
    if not (check b) then 
      let currentScreen = ScreenRecognition.recognizeScreen(w.Bitmap)
      if currentScreen = screen then
        repeat()

  let executeClickAction window s b =
    let preconditionMet = 
      match b.Name with
      | "SitBack" -> 
        let w = InteractionFacade.GetWindow(window, new System.Drawing.Size(650, 490))
        ScreenRecognition.isHeroSitout(w.Bitmap)
      | _ -> true
    if preconditionMet then
      let (x, y, w, h) = b.Region
      let rec imp attempts =
        let l = InteractionFacade.Focus(window)
        Clicker.clickRegion (l.X + x + w / 10, l.Y + y + h / 10, l.X + x + w * 9 / 10, l.Y + y + h * 9 / 10)
        Thread.Sleep(100)
        if attempts > 1 then
          if b.Name = "Max" && s.HeroStack.IsSome then
            ensureAmount window s (fun b -> b <> null && int(b) >= s.HeroStack.Value) (fun () -> imp (attempts-1))
      imp 3

  let enterAmount window s i =
    let rec imp attempts =
      executeClickAction window s { Region = (605, 407, 12, 9); Name = "AmountInput" }
      Clicker.backspace 3
      Clicker.enterText <| i.ToString()
      Thread.Sleep(100)
      if attempts > 1 then        
        ensureAmount window s (fun b -> b = i.ToString()) (fun () -> imp (attempts-1))
    imp 3

  let executeAction window s action =
    match action with
    | Click x -> executeClickAction window s x
    | Amount x -> enterAmount window s x

  let r = new Random()
  let click' msg =
    let rec imp attempts =
      msg.Clicks |> Array.iter (executeAction msg.WindowTitle msg.Screen)
      if attempts > 1 then
        // Check that button click OK
        let w = InteractionFacade.GetWindow(msg.WindowTitle, new System.Drawing.Size(650, 490))
        let s = ScreenRecognition.recognizeScreen(w.Bitmap)
        if s = msg.Screen then imp (attempts-1)
    if not msg.IsInstant then 
      let dmin, dmax =
        match InteractionFacade.GetWindowCount("Heads Up ") with
        | 1 -> (1000, 2400)
        | 2 -> (600, 1900)
        | _ -> (200, 1000)
      Thread.Sleep(r.Next(dmin, dmax))
    imp 3
    // Move to random place below
    Thread.Sleep(150)
    Clicker.shiftBy (100, 50, 150, 100)

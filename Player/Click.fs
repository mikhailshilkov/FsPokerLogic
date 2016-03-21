namespace Player

open System
open System.Threading
open Interaction
open Akka.FSharp

module Click =

  type ClickTarget = (int * int * int * int)
  type ClickAction =
    | Click of ClickTarget
    | Amount of int

  type ClickerMessage = {
    WindowTitle: string
    Clicks: ClickAction[]
    IsInstant: bool
  }

  let executeClickAction window (x, y, w, h) =
    let l = InteractionFacade.Focus(window)
    Clicker.clickRegion (l.X + x + w / 10, l.Y + y + h / 10, l.X + x + w * 9 / 10, l.Y + y + h * 9 / 10)
    Thread.Sleep(300)

  let enterAmount i =
    Clicker.backspace 3
    Clicker.enterText <| i.ToString()

  let executeAction window action =
    match action with
    | Click x -> executeClickAction window x
    | Amount x -> enterAmount x

  let r = new Random()
  let click' msg =
    if not msg.IsInstant then Thread.Sleep(r.Next(1000, 2700))
    msg.Clicks |> Array.iter (executeAction msg.WindowTitle)
    // Move to random place below
    Clicker.shiftBy (100, 50, 150, 100)

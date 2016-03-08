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
  }

  let executeClickAction window (x, y, w, h) =
    Thread.Sleep(300)
    let l = InteractionFacade.Focus(window)
    Clicker.clickRegion (l.X + x, l.Y + y, l.X + x + w, l.Y + y + h)

  let executeAction window action =
    match action with
    | Click x -> executeClickAction window x
    | Amount x -> failwith "not implemented"

  let r = new Random()
  let click' msg =
    Thread.Sleep(r.Next(700, 2400))
    msg.Clicks |> Array.iter (executeAction msg.WindowTitle)
    // Move to random place below
    Thread.Sleep(300)
    Clicker.shiftBy (100, 50, 150, 100)

  let clickActor () =
    let imp _ (msg: ClickerMessage) = click' msg
    imp
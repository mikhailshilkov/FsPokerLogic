namespace Player

open System
open Interaction

module Enable =
  let mutable i = 1
  let handler () =
    i <- (i + 1) % 2
    Console.WriteLine(if i = 0 then "Playing paused..." else "Playing resumed...")

  HotKeyManager.RegisterHotKey(System.Windows.Forms.Keys.Z, KeyModifiers.Control ||| KeyModifiers.Shift) |> ignore
  HotKeyManager.HotKeyPressed.AddHandler(fun _ _ -> handler())
    
  let enableActor _ = i



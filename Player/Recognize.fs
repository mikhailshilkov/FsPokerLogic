namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 120; HeroStack = Some 460; VillainStack = Some 420; HeroBet = None; VillainBet = Some 40; HeroHand = "Qh4c"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; Board = "KcJh6h"; Sitout = Unknown; VillainName = "Ctid80" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    if not(Array.isEmpty result.Actions) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb)) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    
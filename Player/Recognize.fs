namespace Player

open System
open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
//    { TotalPot = Some 240; HeroStack = Some 380; VillainStack = Some 380; HeroBet = None; VillainBet = None; HeroHand = "6s5s"; Board = "8s7cTcJh"; 
//      Button = Villain; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname" }

    { TotalPot = Some 60; HeroStack = Some 520; VillainStack = Some 420; HeroBet = Some 20; VillainBet = Some 40; HeroHand = "3h5h"; Board = null; 
      Button = Villain; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    let hasHand = String.IsNullOrEmpty result.HeroHand |> not
    if result.Sitout = Hero ||
      (hasHand && (Array.length result.Actions > 1) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb))) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    
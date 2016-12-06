namespace Player
 
module Find =
  open Interaction
  open System.Collections.Generic
  open Akka.FSharp
  open System.Drawing
  open Akka.Actor

  let ipoker =
    new WindowExtractor("ipoker", fun title -> 
      let parts = title.Split('-');
      if title.StartsWith("Heads Up") && parts.Length >= 3 then parts.[2].Trim() else null
    )

  let winamax =
    new WindowExtractor("winamax", fun title -> 
      let startIndex = title.IndexOf("Heads-Up(") + 9;
      let endIndex = title.IndexOf(")#");
      if startIndex > 10 && endIndex > startIndex then title.Substring(startIndex, endIndex - startIndex) else null
    )

  let findWindows' () =
    let screenSize = new Size(650, 490)
    let targetSize = new Size(650, 490)
    //let targetSize = new Size(433, 328)
    InteractionFacade.GetWindowList(screenSize, targetSize, ipoker, winamax)

  let findWindowsMock () =
    [ new WindowInfo(TableName = "Fake", Title = "Fake Title") ] |> seq

  let findActor msg = 
    if msg > 0 then 
      //printfn "Finding tables..."
      findWindows' ()
        |> Seq.map (fun x -> ("recognizer-actor-" + x.TableName, x))
    else Seq.empty
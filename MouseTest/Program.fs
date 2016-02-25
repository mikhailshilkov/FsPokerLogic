open System
open System.Windows.Forms
open WindowsInput

let currentPosition () = 
  let mp = Control.MousePosition;
  (mp.X, mp.Y)

let moveTo x y =
  let simulator = new InputSimulator()
  let toX = 65535. * x / (Screen.PrimaryScreen.Bounds.Width |> float)
  let toY = 65535. * y / (Screen.PrimaryScreen.Bounds.Height |> float)
  simulator.Mouse.MoveMouseTo(toX, toY)

let linearStep from until max i =
  from + (until - from) * i / max

let sinStep (from:int) (until:int) (max:int) (index:int) =
  let fromf = from |> float
  let untilf = until |> float
  let maxf = max |> float
  let indexf = index |> float
  fromf + (untilf - fromf) * Math.Sin(Math.PI / 2. * indexf / maxf) |> int

let moveToWorkflow step (toX, toY) = async {
  let (fromX, fromY) = currentPosition()
  let count = Math.Max(10, (Math.Abs (toX - fromX) + Math.Abs (toY - fromY)) / 20)
  for i = 0 to count do
    let x = step fromX toX count i |> float
    let y = step fromY toY count i |> float
    moveTo x y
    do! Async.Sleep 3
  }

let clickRegion (minX, minY, maxX, maxY) =
  let r = new Random()
  let p = (r.Next(minX, maxX), r.Next(minY, maxY))
  moveToWorkflow sinStep p |> Async.RunSynchronously

[<EntryPoint>]
let main argv = 
  clickRegion (100, 200, 200, 250)
  Console.ReadKey |> ignore
  0 // return an integer exit code

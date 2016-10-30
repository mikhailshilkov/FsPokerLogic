namespace Player

open Microsoft.FSharp.Core
open System.Net.Http

module Stats =
  open System.Text

  type StatsMessage = {
    Source: string
  }

  let client = new HttpClient()
  let uri = "https://fspokerlogic.azurewebsites.net/api/PostValue"
  let formatBody s = 
    new StringContent(sprintf "{ 'source': '%s' }" s, Encoding.UTF8, "application/json")
  let runSync t = t |> Async.AwaitTask |> Async.RunSynchronously |> ignore

  let stats' msg =
    msg.Source
    |> Option.ofString
    |> Option.iter (fun s -> 
      printfn "Stats source: %s" s
      let body = formatBody s
      client.PostAsync(uri, body) |> runSync      
    )
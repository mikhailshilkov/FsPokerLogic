namespace Microsoft.FSharp.Core

module Option =
  let add f o = o |> Option.map (fun o -> o, f o)
  let mapFst f o = o |> Option.map (fun (a, b) -> f a, b)


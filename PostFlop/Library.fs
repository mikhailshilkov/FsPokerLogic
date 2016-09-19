namespace Microsoft.FSharp.Core

module Option =
  let addValue v o = o |> Option.map (fun o -> o, v)
  let add f o = o |> Option.map (fun o -> o, f o)
  let mapFst f o = o |> Option.map (fun (a, b) -> f a, b)


namespace PostFlop

module Parsing =
  open System.Globalization

  let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

  let (|StartsWith|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        s.Substring(p.Length) |> Some
    else
        None

  let parseInt (s: string) = 
    match s with
    | Int i -> Some i
    | _ -> None

  let (|Decimal|_|) (str: string) =
   match System.Decimal.TryParse(str.Replace(",", "."), NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture) with
   | (true,f) -> Some(f)
   | _ -> None

  let (|DecimalPerc|_|) (str: string) =
   if str.EndsWith("%") then match str.Replace("%", "") with | Decimal d -> Some d | _ -> None else None

  let parseDecimal (s: string) = 
    match s with
    | Decimal f -> Some f
    | _ -> None

  let parseDecimalThrowing (s: string) = 
    match s with
    | Decimal f -> f
    | _ -> failwith ("Failed to parse " + s)

  let (|SplittedTuple|_|) c (s: string) =
   let parts = s.Split([|c|], 2)
   match parts with
   | [| a; b |] -> Some(a, b)
   | _ -> None

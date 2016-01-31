namespace Recognition

open System
open System.Drawing

module StringRecognition =
  type BW = B | W

  let zero  = [[B;W;W;W;W;W;W;B];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[B;W;W;W;W;W;W;B]]
  let one   = [[B;W;B;B;B;B;B;W];[W;W;W;W;W;W;W;W];[B;B;B;B;B;B;B;W]]
  let two   = [[B;W;B;B;B;B;W;W];[W;B;B;B;B;W;B;W];[W;B;B;B;W;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;B;B;B;W]]
  let three = [[B;W;B;B;B;B;W;B];[W;B;B;B;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]]
  let four  = [[B;B;B;W;W;B;B;B];[B;B;W;B;W;B;B;B];[B;W;B;B;W;B;B;B];[W;W;W;W;W;W;W;W];[B;B;B;B;W;B;B;B]]
  let five  = [[W;W;W;W;B;B;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;B;W;W;W;B]]
  let six   = [[B;B;W;W;W;W;W;B];[B;W;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;B;B;B;W;W;W;B]]
  let seven = [[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;W;W];[W;B;B;B;W;W;B;B];[W;B;W;W;B;B;B;B];[W;W;B;B;B;B;B;B]]
  let eight = [[B;W;W;B;W;W;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]]
  let nine  = [[B;W;W;W;B;B;B;B];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;W;B];[B;W;W;W;W;W;B;B]]

  let comma = [[B;B;B;B;B;B;B;B];[B;B;B;B;B;B;W;W]]

  let compareToDigit (d : list<list<BW>>) (mask: list<list<BW>>) = 
    let equal = 
      mask.Length = d.Length &&
      mask |> List.zip d |> List.forall (fun (x,y) -> Seq.zip x y |> Seq.forall (fun (e,i) -> e = i))
    if equal then Some(1) else None

  let (|Zero|_|) (mask: list<list<BW>>) = compareToDigit zero mask
  let (|One|_|) (mask: list<list<BW>>) = compareToDigit one mask
  let (|Two|_|) (mask: list<list<BW>>) = compareToDigit two mask
  let (|Three|_|) (mask: list<list<BW>>) = compareToDigit three mask
  let (|Four|_|) (mask: list<list<BW>>) = compareToDigit four mask
  let (|Five|_|) (mask: list<list<BW>>) = compareToDigit five mask
  let (|Six|_|) (mask: list<list<BW>>) = compareToDigit six mask
  let (|Seven|_|) (mask: list<list<BW>>) = compareToDigit seven mask
  let (|Eight|_|) (mask: list<list<BW>>) = compareToDigit eight mask
  let (|Nine|_|) (mask: list<list<BW>>) = compareToDigit nine mask

  let (|Comma|_|) (mask: list<list<BW>>) = compareToDigit comma mask

  let matchSymbol (mask: list<list<BW>>) =
    match mask with
    | Zero _ -> '0'
    | One _ -> '1'
    | Two _ -> '2'
    | Three _ -> '3'
    | Four _ -> '4'
    | Five _ -> '5'
    | Six _ -> '6'
    | Seven _ -> '7'
    | Eight _ -> '8'
    | Nine _ -> '9'
    | Comma _ -> ','
    | _ -> '?'

  let recognizeString getPixel width height =
    let isWhite (c : Color) =
      if c.B > 127uy && c.G > 127uy && c.R > 127uy then W
      else B

    let isSeparator (e : list<BW>) = List.forall ((=) B) e

    let removeTrailingSeparators l = Seq.skipWhile (fun x -> Seq.forall ((=) B) x) l
    let removeVerticalPadding l = 
      l
      |> removeTrailingSeparators
      |> Seq.rev
      |> removeTrailingSeparators
      |> Seq.rev

    let removePadding pixels =
        let maxWidth = Array2D.length1 pixels - 1
        let maxHeight = Array2D.length2 pixels - 1
        let first = [0..maxHeight] |> Seq.tryFindIndex (fun x -> Array.exists ((=) W) pixels.[0..maxWidth, x])
        let last = [0..maxHeight] |> Seq.tryFindIndexBack (fun x -> Array.exists ((=) W) pixels.[0..maxWidth, x])

        match (first, last) with
        | (Some f, Some l) ->
          [0..maxWidth] 
          |> Seq.map (fun x -> [f..l] |> Seq.map (fun y -> pixels.[x, y]) |> List.ofSeq)
          |> removeVerticalPadding
        | _ -> Seq.empty


    let splitIntoSymbols (e : list<BW>) (state: list<list<list<BW>>>) = 
      match state with
      | cur::rest ->
          if isSeparator e then
            match cur with
            | _::_ -> []::state // add new list
            | _ -> state        // skip if we already have empty item
          else (e::cur)::rest   // add e to current list
      | _ -> [[e]]

    let pixelColumns = 
      Array2D.init width height (fun x y -> isWhite (getPixel x y))
      |> removePadding

    Seq.foldBack splitIntoSymbols pixelColumns []
    |> List.map matchSymbol
    |> Array.ofSeq
    |> String.Concat

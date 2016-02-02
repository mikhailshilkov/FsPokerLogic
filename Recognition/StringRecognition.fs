namespace Recognition

open System
open System.Drawing

module StringRecognition =
  type BW = B | W
  type CharPattern = {
    Char: char
    Pattern: BW list list
  }

  let numberPatterns = [|  
    { Char = '0'; Pattern = [[B;W;W;W;W;W;W;B];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[B;W;W;W;W;W;W;B]] }
    { Char = '1'; Pattern = [[B;W;B;B;B;B;B;W];[W;W;W;W;W;W;W;W];[B;B;B;B;B;B;B;W]] }
    { Char = '2'; Pattern = [[B;W;B;B;B;B;W;W];[W;B;B;B;B;W;B;W];[W;B;B;B;W;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;B;B;B;W]] }
    { Char = '3'; Pattern = [[B;W;B;B;B;B;W;B];[W;B;B;B;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]] }
    { Char = '4'; Pattern = [[B;B;B;W;W;B;B;B];[B;B;W;B;W;B;B;B];[B;W;B;B;W;B;B;B];[W;W;W;W;W;W;W;W];[B;B;B;B;W;B;B;B]] }
    { Char = '5'; Pattern = [[W;W;W;W;B;B;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;B;W;W;W;B]] }
    { Char = '6'; Pattern = [[B;B;W;W;W;W;W;B];[B;W;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;B;B;B;W;W;W;B]] }
    { Char = '7'; Pattern = [[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;W;W];[W;B;B;B;W;W;B;B];[W;B;W;W;B;B;B;B];[W;W;B;B;B;B;B;B]] }
    { Char = '8'; Pattern = [[B;W;W;B;W;W;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]] }
    { Char = '9'; Pattern = [[B;W;W;W;B;B;B;B];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;W;B];[B;W;W;W;W;W;B;B]] }
    { Char = ','; Pattern = [[B;B;B;B;B;B;B;B];[B;B;B;B;B;B;W;W]] }
  |]

  let buttonPatterns = [|  
    { Char = 'F'; Pattern = [[B;W;W;W;W;W;W;W;W];[B;W;W;W;W;W;W;W;W];[B;W;B;B;W;B;B;B;B];[B;W;B;B;W;B;B;B;B];[B;W;B;B;W;B;B;B;B];[B;W;B;B;W;B;B;B;B]] }
    { Char = 'o'; Pattern = [[B;B;B;B;W;W;W;W;B];[B;B;B;W;W;W;W;W;W];[B;B;B;W;B;B;B;B;W];[B;B;B;W;B;B;B;B;W];[B;B;B;W;W;W;W;W;W];[B;B;B;B;W;W;W;W;B]] }
    { Char = 'l'; Pattern = [[W;W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W;W]] }
    { Char = 'd'; Pattern = [[B;B;B;B;W;W;W;W;B];[B;B;B;W;W;W;W;W;W];[B;B;B;W;B;B;B;B;W];[B;B;B;W;B;B;B;B;W];[W;W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W;W]] }
    { Char = 'C'; Pattern = [[B;B;B;W;W;W;W;B;B];[B;B;W;W;W;W;W;W;B];[B;W;W;B;B;B;B;W;W];[B;W;B;B;B;B;B;B;W];[B;W;B;B;B;B;B;B;W];[B;W;B;B;B;B;B;B;W];[B;W;B;B;B;B;B;B;W]] }
    { Char = 'a'; Pattern = [[B;B;B;B;B;B;W;W;B];[B;B;B;W;B;W;W;W;W];[B;B;B;W;B;W;B;B;W];[B;B;B;W;B;W;B;B;W];[B;B;B;W;W;W;W;W;W];[B;B;B;B;W;W;W;W;W]] }
    { Char = 'R'; Pattern = [[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[W;B;B;B;W;B;B;B];[W;B;B;B;W;W;B;B];[W;W;W;W;W;W;W;B];[B;W;W;W;B;B;W;W];[B;B;B;B;B;B;B;W]] }    
    { Char = 'a'; Pattern = [[B;B;B;B;B;W;W;B];[B;B;W;B;W;W;W;W];[B;B;W;B;W;B;B;W];[B;B;W;B;W;B;B;W];[B;B;W;W;W;W;W;W];[B;B;B;W;W;W;W;W]] }
    { Char = 'i'; Pattern = [[W;B;W;W;W;W;W;W];[W;B;W;W;W;W;W;W]] }
    { Char = 's'; Pattern = [[B;B;B;W;W;B;B;W];[B;B;W;W;W;B;B;W];[B;B;W;B;W;W;B;W];[B;B;W;B;B;W;W;W];[B;B;W;B;B;W;W;B]] }
    { Char = 'e'; Pattern = [[B;B;B;W;W;W;W;B];[B;B;W;W;W;W;W;W];[B;B;W;B;W;B;B;W];[B;B;W;B;W;B;B;W];[B;B;W;W;W;B;W;W];[B;B;B;W;W;B;W;B]] }
    { Char = 'T'; Pattern = [[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;B;B];[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;B;B]] }
    { Char = 'o'; Pattern = [[B;B;B;W;W;W;W;B];[B;B;W;W;W;W;W;W];[B;B;W;B;B;B;B;W];[B;B;W;B;B;B;B;W];[B;B;W;W;W;W;W;W];[B;B;B;W;W;W;W;B]] }
    { Char = 'h'; Pattern = [[W;W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W;W];[B;B;B;W;B;B;B;B;B];[B;B;B;W;B;B;B;B;B];[B;B;B;W;W;W;W;W;W];[B;B;B;B;W;W;W;W;W]] }
    { Char = 'e'; Pattern = [[B;B;B;B;W;W;W;W;B];[B;B;B;W;W;W;W;W;W];[B;B;B;W;B;W;B;B;W];[B;B;B;W;B;W;B;B;W];[B;B;B;W;W;W;B;W;W];[B;B;B;B;W;W;B;W;B]] }
    { Char = 'c'; Pattern = [[B;B;B;B;W;W;W;W;B];[B;B;B;W;W;W;W;W;W];[B;B;B;W;B;B;B;B;W];[B;B;B;W;B;B;B;B;W];[B;B;B;W;B;B;B;B;W]] }
    { Char = 'k'; Pattern = [[W;W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W;W];[B;B;B;B;W;W;W;B;B];[B;B;B;W;W;B;W;W;B];[B;B;B;W;B;B;B;W;W];[B;B;B;B;B;B;B;B;W]] }
    { Char = 'A'; Pattern = [[B;B;B;B;B;B;W;W;W];[B;B;B;W;W;W;W;W;W];[B;W;W;W;W;W;W;B;B];[B;W;W;B;B;B;W;B;B];[B;W;W;W;W;W;W;B;B];[B;B;B;W;W;W;W;W;W];[B;B;B;B;B;B;W;W;W]] }
    { Char = 'I'; Pattern = [[B;W;B;B;B;B;B;B;W];[B;W;W;W;W;W;W;W;W];[B;W;W;W;W;W;W;W;W];[B;W;B;B;B;B;B;B;W]] }
    { Char = 'n'; Pattern = [[B;B;B;W;W;W;W;W;W];[B;B;B;W;W;W;W;W;W];[B;B;B;W;B;B;B;B;B];[B;B;B;W;B;B;B;B;B];[B;B;B;W;W;W;W;W;W];[B;B;B;B;W;W;W;W;W]] }
  |]

  let getChar patterns bws =
    let samePatterns h p =
      Seq.zip h p
      |> Seq.forall (fun (v1, v2) -> v1 = v2)
    let matchingPattern = 
      patterns 
        |> Array.filter (fun p -> List.length p.Pattern = List.length bws)
        |> Array.filter (fun p -> samePatterns bws p.Pattern)
        |> Array.tryHead
    defaultArg (Option.map (fun p -> p.Char) matchingPattern) '?'

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

  let isWhite (c : Color) =
    if c.B > 127uy && c.G > 127uy && c.R > 127uy then W
    else B

  let recognizeString (matchSymbol: BW list list -> char) getPixel width height =
    let isSeparator (e : list<BW>) = List.forall ((=) B) e

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

  let recognizeNumber x =
    recognizeString (getChar numberPatterns) x

  let recognizeButton x =
    recognizeString (getChar buttonPatterns) x

  let parsePattern getPixel width height =
    seq { for x in 0 .. width - 1 do
            yield seq { for y in 0 .. height - 1 do yield isWhite (getPixel x y)} 
        }
    |> Seq.map (fun y -> "[" + (y|> Seq.map (fun x -> if x = B then "B" else "W") |> String.concat ";") + "]")
    |> String.concat ";"

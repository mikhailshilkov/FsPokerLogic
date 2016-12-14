module Rooms

open Interaction

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



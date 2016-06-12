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
    { Char = ','; Pattern = [[B;B;B;B;B;B;W;W]] }
  |]

  let number9Patterns = [|  
    { Char = '0'; Pattern = [[B;W;W;W;W;W;W;W;B];[W;B;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;B;W];[B;W;W;W;W;W;W;W;B]] }
    { Char = '1'; Pattern = [[B;W;B;B;B;B;B;B;W];[B;W;B;B;B;B;B;B;W];[W;W;W;W;W;W;W;W;W];[B;B;B;B;B;B;B;B;W];[B;B;B;B;B;B;B;B;W]] }
    { Char = '2'; Pattern = [[B;W;B;B;B;B;B;W;W];[W;B;B;B;B;B;W;B;W];[W;B;B;B;B;W;B;B;W];[W;B;B;B;B;W;B;B;W];[W;B;B;B;W;B;B;B;W];[B;W;W;W;B;B;B;B;W]] }
    { Char = '3'; Pattern = [[B;W;B;B;B;B;B;W;B];[W;B;B;B;B;B;B;B;W];[W;B;B;B;W;B;B;B;W];[W;B;B;B;W;B;B;B;W];[W;B;B;B;W;B;B;B;W];[B;W;W;W;B;W;W;W;B]] }
    { Char = '4'; Pattern = [[B;B;B;B;W;W;B;B;B];[B;B;B;W;B;W;B;B;B];[B;B;W;B;B;W;B;B;B];[B;W;B;B;B;W;B;B;B];[W;W;W;W;W;W;W;W;W];[B;B;B;B;B;W;B;B;B]] }
    { Char = '5'; Pattern = [[W;W;W;W;B;B;B;W;B];[W;B;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[W;B;B;B;W;W;W;W;B]] }
    { Char = '6'; Pattern = [[B;B;W;W;W;W;W;W;B];[B;W;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[W;B;B;W;B;B;B;B;W];[B;B;B;B;W;W;W;W;B]] }
    { Char = '7'; Pattern = [[W;B;B;B;B;B;B;B;B];[W;B;B;B;B;B;B;B;W];[W;B;B;B;B;B;W;W;B];[W;B;B;B;W;W;B;B;B];[W;B;W;W;B;B;B;B;B];[W;W;B;B;B;B;B;B;B]] }
    { Char = '8'; Pattern = [[B;W;W;W;B;W;W;W;B];[W;B;B;B;W;B;B;B;W];[W;B;B;B;W;B;B;B;W];[W;B;B;B;W;B;B;B;W];[W;B;B;B;W;B;B;B;W];[B;W;W;W;B;W;W;W;B]] }
    { Char = '9'; Pattern = [[B;W;W;W;W;B;B;B;B];[W;B;B;B;B;W;B;B;W];[W;B;B;B;B;W;B;B;W];[W;B;B;B;B;W;B;B;W];[W;B;B;B;B;W;B;W;B];[B;W;W;W;W;W;W;B;B]] }
  |]

  let blindNumberPatterns = [|  
    { Char = '0'; Pattern = [[B;W;W;W;W;W;W;B];[W;W;W;W;W;W;W;W];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[W;W;W;W;W;W;W;W];[B;W;W;W;W;W;W;B]] }
    { Char = '1'; Pattern = [[B;W;B;B;B;B;B;W];[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[B;B;B;B;B;B;B;W]] }
    { Char = '2'; Pattern = [[B;W;B;B;B;B;W;W];[W;W;B;B;B;W;W;W];[W;B;B;B;W;W;B;W];[W;B;B;W;W;B;B;W];[W;W;W;W;B;B;B;W];[B;W;W;B;B;B;B;W]] }
    { Char = '3'; Pattern = [[B;W;B;B;B;B;W;B];[W;W;B;B;B;B;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;W;W;W;W;W;W;W];[B;W;W;B;W;W;W;B]] }
    { Char = '4'; Pattern = [[B;B;B;W;W;B;B;B];[B;B;W;W;W;B;B;B];[B;W;W;B;W;B;B;B];[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[B;B;B;B;W;B;B;B]] }
    { Char = '5'; Pattern = [[W;W;W;W;B;B;W;B];[W;W;W;W;B;B;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;W;W;W;W];[W;B;B;B;W;W;W;B]] }
    { Char = '6'; Pattern = [[B;B;W;W;W;W;W;B];[B;W;W;W;W;W;W;W];[W;W;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;W;W;W;W];[B;B;B;B;W;W;W;B]] }
    { Char = '7'; Pattern = [[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;B;B];[W;B;B;B;B;W;W;W];[W;B;W;W;W;W;W;W];[W;W;W;W;W;B;B;B];[W;W;B;B;B;B;B;B]] }
    { Char = '8'; Pattern = [[B;W;W;B;W;W;W;B];[W;W;W;W;W;W;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;W;W;W;W;W;W;W];[B;W;W;B;W;W;W;B]] }
    { Char = '9'; Pattern = [[B;W;W;W;B;B;B;B];[W;W;W;W;W;B;B;W];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;B;W];[W;W;W;W;W;W;W;W];[B;W;W;W;W;W;W;B]] }
    { Char = '/'; Pattern = [[B;B;B;B;B;B;B;W];[B;B;W;W;W;W;W;W];[W;W;W;W;W;W;W;B];[W;W;B;B;B;B;B;B]] }
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
    { Char = 'B'; Pattern = [[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;W;W;W;W;W;W;W];[B;W;W;B;W;W;W;B]] }
    { Char = 't'; Pattern = [[W;W;W;W;W;W;W;B];[W;W;W;W;W;W;W;W];[B;B;W;B;B;B;B;W];[B;B;W;B;B;B;B;W]] }

    // Sitout popup
    { Char = 'Y'; Pattern = [[W;W;B;B;B;B;B;B];[B;B;W;W;B;B;B;B];[B;B;B;B;W;W;W;W];[B;B;W;W;B;B;B;B];[W;W;B;B;B;B;B;B]] }
    { Char = 'E'; Pattern = [[W;W;W;W;W;W;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;B;B;B;B;W]] }
    { Char = 'S'; Pattern = [[B;W;W;B;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;B;W;W;W;B]] }

    // Sitout popup Titan
    { Char = 'Y'; Pattern = [[W;W;B;B;B;B;B;B];[W;W;W;W;B;B;B;B];[B;B;W;W;W;W;W;W];[B;B;W;W;W;W;W;W];[W;W;W;W;B;B;B;B];[W;W;B;B;B;B;B;B]] }
    { Char = 'E'; Pattern = [[W;W;W;W;W;W;W;W];[W;W;W;W;W;W;W;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W]] }
    { Char = 'S'; Pattern = [[B;W;W;W;B;B;B;W];[W;W;W;W;W;B;B;W];[W;B;B;W;W;B;B;W];[W;B;B;W;W;B;B;W];[W;B;B;W;W;W;W;W];[W;B;B;B;W;W;W;B]] }
  |]

  let textPatterns = [|  
    { Char = 'A'; Pattern = [[B;B;B;B;B;W;W;W;B;B]; [B;B;W;W;W;W;B;B;B;B]; [W;W;B;B;B;W;B;B;B;B]; [W;W;B;B;B;W;B;B;B;B]; [B;B;W;W;W;W;B;B;B;B]; [B;B;B;B;B;W;W;W;B;B]] }
    { Char = 'B'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [B;W;W;B;W;W;W;B;B;B]] }  
    { Char = 'C'; Pattern = [[B;B;W;W;W;W;B;B;B;B]; [B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]] }
    { Char = 'D'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [B;W;B;B;B;B;W;B;B;B]; [B;B;W;W;W;W;B;B;B;B]] }
    { Char = 'E'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]] }
    { Char = 'F'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;W;B;B;B;B;B;B]; [W;B;B;W;B;B;B;B;B;B]; [W;B;B;W;B;B;B;B;B;B]; [W;B;B;W;B;B;B;B;B;B]] }
    { Char = 'G'; Pattern = [[B;B;W;W;W;W;B;B;B;B]; [B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;W;B;B;W;B;B]; [W;B;B;B;W;B;B;W;B;B]; [W;B;B;B;W;W;W;W;B;B]] }
    { Char = 'H'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;B;W;B;B;B;B;B;B]; [B;B;B;W;B;B;B;B;B;B]; [B;B;B;W;B;B;B;B;B;B]; [B;B;B;W;B;B;B;B;B;B]; [W;W;W;W;W;W;W;W;B;B]] }
    { Char = 'I'; Pattern = [[W;B;B;B;B;B;B;W;B;B]; [W;W;W;W;W;W;W;W;B;B]; [W;B;B;B;B;B;B;W;B;B]] }
    { Char = 'J'; Pattern = [[B;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;W;W;W;W;W;W;B;B;B]] }    
    { Char = 'K'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;B;W;W;B;B;B;B;B]; [B;B;W;B;B;W;B;B;B;B]; [B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]] }
    { Char = 'L'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]] }
    { Char = 'M'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;W;B;B;B;B;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]; [W;W;B;B;B;B;B;B;B;B]; [W;W;W;W;W;W;W;W;B;B]] }
    { Char = 'N'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;W;B;B;B;B;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [W;W;W;W;W;W;W;W;B;B]] }
    { Char = 'O'; Pattern = [[B;B;W;W;W;W;B;B;B;B]; [B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [B;W;B;B;B;B;W;B;B;B]; [B;B;W;W;W;W;B;B;B;B]] }
    { Char = 'P'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;B;W;B;B;B;B;B]; [W;B;B;B;W;B;B;B;B;B]; [W;B;B;B;W;B;B;B;B;B]; [B;W;W;W;B;B;B;B;B;B]] }
    { Char = 'Q'; Pattern = [[B;B;W;W;W;W;B;B;B;B]; [B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;W;B]; [B;W;B;B;B;B;W;B;B;W]; [B;B;W;W;W;W;B;B;B;W]] }
    { Char = 'R'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [W;B;B;B;W;B;B;B;B;B]; [W;B;B;B;W;B;B;B;B;B]; [W;B;B;B;W;W;B;B;B;B]; [B;W;W;W;B;B;W;B;B;B]; [B;B;B;B;B;B;B;W;B;B]] }
    { Char = 'S'; Pattern = [[B;W;W;B;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;B;W;W;W;B;B;B]] }
    { Char = 'T'; Pattern = [[W;B;B;B;B;B;B;B;B;B]; [W;B;B;B;B;B;B;B;B;B]; [W;W;W;W;W;W;W;W;B;B]; [W;B;B;B;B;B;B;B;B;B]; [W;B;B;B;B;B;B;B;B;B]] }
    { Char = 'U'; Pattern = [[W;W;W;W;W;W;W;B;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [W;W;W;W;W;W;W;B;B;B]] }
    { Char = 'V'; Pattern = [[W;W;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;W;W;W;B;B;B;B]; [W;W;W;B;B;B;B;B;B;B]] }
    { Char = 'W'; Pattern = [[W;W;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;W;W;W;B;B;B;B]; [W;W;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;W;W;W;B;B;B;B]; [W;W;W;B;B;B;B;B;B;B]] }
    { Char = 'X'; Pattern = [[W;W;B;B;B;B;W;W;B;B]; [B;B;W;B;B;W;B;B;B;B]; [B;B;B;W;W;B;B;B;B;B]; [B;B;W;B;B;W;B;B;B;B]; [W;W;B;B;B;B;W;W;B;B]] }
    { Char = 'Y'; Pattern = [[W;W;B;B;B;B;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;W;W;B;B]; [B;B;W;W;B;B;B;B;B;B]; [W;W;B;B;B;B;B;B;B;B]] }
    { Char = 'Z'; Pattern = [[W;B;B;B;B;B;W;W;B;B]; [W;B;B;B;B;W;B;W;B;B]; [W;B;B;W;W;B;B;W;B;B]; [W;B;W;B;B;B;B;W;B;B]; [W;W;B;B;B;B;B;W;B;B]] }
    { Char = 'a'; Pattern = [[B;B;B;B;B;W;W;B;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;B;W;W;W;W;W;B;B]] }
    { Char = 'b'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;B;W;W;W;W;B;B;B]] }
    { Char = 'c'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]] }
    { Char = 'd'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [W;W;W;W;W;W;W;W;B;B]] }
    { Char = 'e'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;B;W;W;B;W;B;B;B]] }
    { Char = 'f'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]] }
    { Char = 'g'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;W]; [B;B;W;B;B;B;B;W;B;W]; [B;B;W;B;B;B;B;W;B;W]; [B;B;W;W;W;W;W;W;W;B]] }
    { Char = 'h'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;W;W;B;B]] }
    { Char = 'i'; Pattern = [[W;B;W;W;W;W;W;W;B;B]] }
    { Char = 'j'; Pattern = [[B;B;W;B;B;B;B;B;B;W]; [W;B;W;W;W;W;W;W;W;B]] }
    { Char = 'k'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;B;B;W;B;B;B;B;B]; [B;B;B;W;B;W;B;B;B;B]; [B;B;W;B;B;B;W;B;B;B]; [B;B;B;B;B;B;B;W;B;B]] }
    { Char = 'l'; Pattern = [[W;W;W;W;W;W;W;W;B;B]] }
    { Char = 'm'; Pattern = [[B;B;W;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;W;W;B;B]] }
    { Char = 'n'; Pattern = [[B;B;W;W;W;W;W;W;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]; [B;B;B;W;W;W;W;W;B;B]] }
    { Char = 'o'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;B;W;W;W;W;B;B;B]] }  
    { Char = 'p'; Pattern = [[B;B;W;W;W;W;W;W;W;W]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;B;W;W;W;W;B;B;B]] }
    { Char = 'q'; Pattern = [[B;B;B;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;W;W;W;W;W;W;W]] }
    { Char = 'r'; Pattern = [[B;B;W;W;W;W;W;W;B;B]; [B;B;B;W;B;B;B;B;B;B]; [B;B;W;B;B;B;B;B;B;B]] }
    { Char = 's'; Pattern = [[B;B;B;W;W;B;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;B;B;W;B;W;B;B]; [B;B;W;B;B;W;W;B;B;B]] }
    { Char = 't'; Pattern = [[W;W;W;W;W;W;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]; [B;B;W;B;B;B;B;W;B;B]] }
    { Char = 'u'; Pattern = [[B;B;W;W;W;W;W;B;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;B;B;B;B;B;W;B;B]; [B;B;W;W;W;W;W;W;B;B]] }
    { Char = 'v'; Pattern = [[B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]] }
    { Char = 'w'; Pattern = [[B;B;W;W;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;B;B;B;B;W;W;B;B]; [B;B;W;W;W;W;B;B;B;B]] }
    { Char = 'x'; Pattern = [[B;B;W;B;B;B;B;W;B;B]; [B;B;B;W;B;B;W;B;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;B;W;B;B;W;B;B;B]; [B;B;W;B;B;B;B;W;B;B]] }
    { Char = 'y'; Pattern = [[B;B;W;W;B;B;B;B;B;B]; [B;B;B;B;W;W;B;B;W;W]; [B;B;B;B;B;B;W;W;B;B]; [B;B;B;B;W;W;B;B;B;B]; [B;B;W;W;B;B;B;B;B;B]] }
    { Char = 'z'; Pattern = [[B;B;W;B;B;B;W;W;B;B]; [B;B;W;B;B;W;B;W;B;B]; [B;B;W;B;W;B;B;W;B;B]; [B;B;W;W;B;B;B;W;B;B]] }
    { Char = '0'; Pattern = [[B;W;W;W;W;W;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;B;B;B;B;W;B;B]; [B;W;W;W;W;W;W;B;B;B]] }
    { Char = '1'; Pattern = [[B;W;B;B;B;B;B;W;B;B]; [W;W;W;W;W;W;W;W;B;B]; [B;B;B;B;B;B;B;W;B;B]] }
    { Char = '2'; Pattern = [[B;W;B;B;B;B;W;W;B;B]; [W;B;B;B;B;W;B;W;B;B]; [W;B;B;B;W;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [B;W;W;B;B;B;B;W;B;B]] }
    { Char = '3'; Pattern = [[B;W;B;B;B;B;W;B;B;B]; [W;B;B;B;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [B;W;W;B;W;W;W;B;B;B]] }
    { Char = '4'; Pattern = [[B;B;B;W;W;B;B;B;B;B]; [B;B;W;B;W;B;B;B;B;B]; [B;W;B;B;W;B;B;B;B;B]; [W;W;W;W;W;W;W;W;B;B]; [B;B;B;B;W;B;B;B;B;B]] }
    { Char = '5'; Pattern = [[W;W;W;W;B;B;W;B;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;B;W;W;W;B;B;B]] }
    { Char = '6'; Pattern = [[B;B;W;W;W;W;W;B;B;B]; [B;W;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [B;B;B;B;W;W;W;B;B;B]] }
    { Char = '7'; Pattern = [[W;B;B;B;B;B;B;B;B;B]; [W;B;B;B;B;B;W;W;B;B]; [W;B;B;B;W;W;B;B;B;B]; [W;B;W;W;B;B;B;B;B;B]; [W;W;B;B;B;B;B;B;B;B]] }
    { Char = '8'; Pattern = [[B;W;W;B;W;W;W;B;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [W;B;B;W;B;B;B;W;B;B]; [B;W;W;B;W;W;W;B;B;B]] }
    { Char = '9'; Pattern = [[B;W;W;W;B;B;B;B;B;B]; [W;B;B;B;W;B;B;W;B;B]; [W;B;B;B;W;B;B;W;B;B]; [W;B;B;B;W;B;W;B;B;B]; [B;W;W;W;W;W;B;B;B;B]] }
  |]

  let noSpacePatterns = [|  
    { Char = 'k'; Pattern = [[W;W;W;W;W;W;W;W;B;B]; [B;B;B;B;W;B;B;B;B;B]; [B;B;B;W;B;W;B;B;B;B]; [B;B;W;B;B;B;W;B;B;B]; [B;B;B;B;B;B;B;W;B;B]] }
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

  let lessThanXWhite x seq =
    (Seq.filter ((=) W) seq |> Seq.length) >= x


  let removePadding threshold minHeight pixels =
      let maxWidth = Array2D.length1 pixels - 1
      let maxHeight = Array2D.length2 pixels - 1
      let firstX = [0..maxWidth] |> Seq.tryFindIndex (fun y -> lessThanXWhite 1 pixels.[y, 0..maxHeight])
      let lastX = [0..maxWidth] |> Seq.tryFindIndexBack (fun y -> lessThanXWhite 1 pixels.[y, 0..maxHeight])
      let firstY = [0..maxHeight] |> Seq.tryFindIndex (fun x -> lessThanXWhite threshold pixels.[0..maxWidth, x])
      let lastY = [0..maxHeight] |> Seq.tryFindIndexBack (fun x -> lessThanXWhite threshold pixels.[0..maxWidth, x])

      match (firstX, lastX, firstY, lastY) with
      | (Some fx, Some lx, Some fy, Some ly) when fy + minHeight - 1 <= maxHeight -> 
        pixels.[fx..lx, fy..(max ly (fy + minHeight - 1))]
      | _ -> Array2D.init 0 0 (fun _ _ -> B)

  let isWhite (c : Color) =
    if c.B > 180uy && c.G > 180uy && c.R > 180uy then W
    else B

  let recognizeString (matchSymbol: BW list list -> char) threshold minHeight getPixel width height =
    let isSeparator (e : list<BW>) = List.forall ((=) B) e

    let invertifWhiteBackground pixels = 
      let mutable whiteCount = 0
      let mutable blackCount = 0
      let whiteBackground = pixels |> Array2D.iter (fun x -> 
        match x with
        | W -> whiteCount <- whiteCount + 1
        | B -> blackCount <- blackCount + 1)
      if whiteCount > blackCount then 
        pixels |> Array2D.map (fun x -> if x = B then W else B)
      else pixels

    let splitIntoSymbols (e : BW list) (state: BW list list list) = 
      match state with
      | cur::rest ->
          if isSeparator e then
            match cur with
            | _::_ -> []::state // add new list
            | _ -> state        // skip if we already have empty item
          else (e::cur)::rest   // add e to current list
      | _ -> [[e]]

    let splitNoSpaceSymbols (symbols: BW list list list) =
      let samePatterns h p =
        List.length h > List.length p
        && Seq.zip h p |> Seq.forall (fun (v1, v2) -> v1 = v2)
      symbols
      |> List.map (fun s ->
        let matchingPattern =
          noSpacePatterns 
          |> Array.filter (fun p -> samePatterns s p.Pattern)
          |> Array.tryHead
        match matchingPattern with
        | Some x -> [x.Pattern; List.skip x.Pattern.Length s]
        | None -> [s])
      |> List.concat

    let pixels = 
      Array2D.init width height (fun x y -> isWhite (getPixel x y))
      |> invertifWhiteBackground
      |> removePadding threshold minHeight
        
    let pixelColumns =
      [0..Array2D.length1 pixels - 1] 
      |> Seq.map (fun x -> pixels.[x, 0..Array2D.length2 pixels - 1] |> List.ofArray)      

    Seq.foldBack splitIntoSymbols pixelColumns []
    |> splitNoSpaceSymbols
    |> List.map matchSymbol
    |> Array.ofSeq
    |> String.Concat

  let recognizeNumber x =
    recognizeString (getChar numberPatterns) 2 8 x

  let recognizeText getPixel (y:int) width height =
    let o = 
      [0..5]
      |> Seq.map (fun dy -> recognizeString (getChar textPatterns) 0 10 (getPixel (y+dy)) width height)
    o |> Seq.maxBy (fun x -> x |> Seq.map (fun c -> match c with | '?' -> 1 | _ -> 5) |> Seq.sum)

  let recognizeBetSize x =
    recognizeString (getChar number9Patterns) 2 9 x

  let recognizeButton x y z =
    let b = recognizeString (getChar buttonPatterns) 2 8 x y z
    if b <> "?" then b else null

  let recognizeBlinds x y z =
    let s = recognizeString (getChar blindNumberPatterns) 3 8 x y z
    s.Replace("?", "")

  let parseStringPattern getPixel width height =
    let a = 
      [0 .. width - 1]
      |> Seq.map (fun x -> 
        let b = [0 .. height - 1] |> Seq.map (fun y -> if isWhite (getPixel x y) = B then "B" else "W") |> String.concat ";"
        "[" + b + "]")
      |> String.concat "; "
    "{ Char = '?'; Pattern = [" + a + "] }\n"

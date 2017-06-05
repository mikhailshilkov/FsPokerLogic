
open Excel
open Excel.Serialization
open Microsoft.Office.Interop.Excel
open System
open System.Runtime.Serialization
open System.Collections.Generic
open System.IO
open System.IO.Compression

let serializer = new DataContractSerializer(typeof<ExcelDatabase>)
let zip books =
  let memoryStream = new MemoryStream()
  use archive = new ZipArchive(memoryStream, ZipArchiveMode.Create, true)
  let demoFile = archive.CreateEntry("data.xml");

  use entryStream = demoFile.Open()
  use streamWriter = new StreamWriter(entryStream)
  serializer.WriteObject(entryStream, books)
  memoryStream

[<EntryPoint>]
let main argv =
//    let fn = "rules_20170406-0947.xml"
//    printf "Testing the file %s... " fn
//    use file2 = File.OpenRead fn
//    let result = serializer.ReadObject(file2)
//    printfn "done." 

    printf "Searching excel files... " 
    let files =
      Directory.GetFiles(System.IO.Directory.GetCurrentDirectory(), "*.xlsx")
    printfn "%i found." files.Length

    printf "Reading files" 
    let books =
      files
      |> Seq.ofArray
      |> Seq.map (fun x -> printf "."; x)
      |> Seq.map (fun name -> name.Substring(name.LastIndexOf "\\" + 1), parseExcel name)      
      |> dict
      |> fun x -> { Files = x }
    printfn " done."

    let newestFile = 
      files 
      |> Array.map (fun n -> File.GetLastWriteTime n)
      |> Array.sortDescending
      |> Array.tryHead

    match newestFile with
    | Some dt ->
      let outfileName = sprintf "rules_%s.zip" (dt.ToString "yyyyMMdd-HHmm")

      printf "Writing file %s... " outfileName

      use memoryStream = zip books
      use fileStream = new FileStream(outfileName, FileMode.Create)
      memoryStream.Seek(0L, SeekOrigin.Begin) |> ignore
      memoryStream.CopyTo(fileStream)
      printfn "done." 
    | None -> ()
      
    printfn "Press any key to exit."
    Console.ReadKey() |> ignore
    0 // return an integer exit code

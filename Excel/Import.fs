namespace Excel

module Import =
  open Microsoft.Office.Interop.Excel
  open System.Runtime.InteropServices

  let openExcel fileName =
    let xlApp = new ApplicationClass()
    (xlApp.Workbooks.Open(fileName), xlApp)

  let closeExcel (xlWorkBook, xlApp: ApplicationClass) =
    Marshal.ReleaseComObject(xlWorkBook) |> ignore
    xlApp.Quit()
    Marshal.ReleaseComObject(xlApp) |> ignore
namespace Excel

module Import =
  open Microsoft.Office.Interop.Excel
  open System.Runtime.InteropServices

  let openExcel fileName =
    let xlApp = new ApplicationClass()
    let wb = xlApp.Workbooks.Open(fileName, 0, "True")
    (wb, xlApp)

  let closeExcel (xlWorkBook: Workbook, xlApp: ApplicationClass) =
    let misValue = System.Reflection.Missing.Value
    xlWorkBook.Close(false, misValue, misValue)
    Marshal.ReleaseComObject(xlWorkBook) |> ignore
    xlApp.Quit()
    Marshal.ReleaseComObject(xlApp) |> ignore
#############################roxygen explaination
#' @name  ExcelToListNA
#' @title  Convert a xlsx file to a list of dataframes retain NAs
#' @usage  ListToExcel(Data = ksearlist, filename = "~/ksear_output.xlsx")
#' @param exfile name of excel focument to put in
#' @export ExcelToListNA
ExcelToListNA <- function(exfile){
  sheets <- readxl::excel_sheets(exfile)
  x <- lapply(sheets, function(X) data.frame(readxl::read_excel(exfile, sheet = X, na = "NA"), stringsAsFactors = F))
  names(x) <- sheets
  return(x)
}

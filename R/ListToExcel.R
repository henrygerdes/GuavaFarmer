#############################roxygen explaination
#' @name  ListToExcel
#' @export ListToExcel
#' @title  Convert a list of dataframes/matrices into an excelfile
#' @usage  ListToExcel(Data = ksearlist, filename = "~/ksear_output.xlsx")
#' @param list List of DataFrames to be converted into an xlsx document(names(list) will be used as sheet names)
#' @param filename path and filename ending with ".xlsx" for the file to be made to
#' @param rowNames if TRUE rownames will be saved in xlsx file

ListToExcel <- function(list, filename, rowNames = T){
  sheets <- names(list)
  wb <- openxlsx::createWorkbook()
  lapply(sheets, FUN = function(x){openxlsx::addWorksheet(wb, sheetName = x)})
  lapply(sheets, FUN = function(x){openxlsx::writeData(wb, sheet =x, x = list[[x]], rowNames = rowNames)})
  openxlsx::saveWorkbook(wb,filename)

  print(paste(paste(filename, "made with sheets:"), paste(c(1:length(list)), ")", sheets, sep = "",collapse =" ")))
}

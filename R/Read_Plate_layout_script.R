#' @name  Read_Plate_layout
#' @title  Read Plate Layout file for Guava
#' @usage Read_Plate_layout("platelayout.xlsx", colwise = TRUE, concentration = FALSE)
#' @param filename Path of the plate layout file
#' @param conc If TRUE concentrations will be imported
#' @export Read_Plate_layoutd


####################################   Read_Plate_layouts     ###############################################################################################################
Read_Plate_layout <- function(filename,
                                conc = FALSE,
                              cell_line = NULL) {
  require(dplyr)

  excel_out <- GuavaFarmer::ExcelToListNA(filename)
  SampleID <-
    expand.grid(formatC(1:12, width = 2, flag = 0), letters[1:8])
  SampleID <-
    paste(SampleID$Var2, SampleID$Var1, sep = "") %>% toupper()

  .PlateLabels <- function(z, conc = conc) {
    .GetDeets <- function(x, y) {
      data.frame(z[x, y], stringsAsFactors = F)
    }
    z <- data.frame(t(z), stringsAsFactors = F)
    out <-
      data.frame(
        Cell.Line = unlist(lapply(
          c(1:8),
          FUN = function(y) {
            .GetDeets(x = (2:13), y)
          }
        )) %>% paste(),
        SampleID = SampleID,
        Drug = unlist(lapply(
          c(1:8),
          FUN = function(y) {
            .GetDeets(x = (15:26), y)
          }
        )) %>% paste(),
        stringsAsFactors = F
      )
    if (conc == T) {
      out$Concentration <-
        unlist(lapply(
          c(1:8),
          FUN = function(y) {
            .GetDeets(x = (28:39), y)
          }
        ))
    }


    if(!is.null(cell_line)){

      out$Cell.Line <- cell_line

      }

    out <- na.omit(out)
    return(out)
  }

  all_labels <-
    lapply(excel_out, function(x) {
      .PlateLabels(z = x, conc = conc) %>% data.frame(stringsAsFactors = F)
    })


  return(all_labels)
}

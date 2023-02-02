#' @name  GuavaSmoothy
#' @export GuavaSmoothy2
#' @title  Import values from VIA files
#' @usage GuavaSmoothy("assay_file.VIA")
#' @param filenames list of filenames you want to be labeled by the template
#' @param easyfit if true values from easyfit will be used
#' @param label labels generated from import labels to be used

GuavaSmoothy <- function(label, filenames, Cell.line = NULL, easyfit = FALSE, annexin = FALSE){
  require(dplyr)
  require(foreach)

  if(easyfit& !annexin){
    inp_cols <- c("Viable.13", "Viable.14", "Dead.13", "Sample.ID")
  }else if (!easyfit & !annexin){
    inp_cols <- c("Viable.2", "Viable.1", "Dead.2", "Sample.ID")
  }else if (easyfit & annexin){
    inp_cols <- c("Viable.2", "Viable.1", "Dead.2", "Sample.ID")
  }else if (!easyfit & annexin){
    inp_cols <- c("Viable.2", "Viable.1", "Dead.2", "Sample.ID")
  }


  out <- foreach(i = filenames, .combine = "rbind")%do%{

    data <- read.csv(i, stringsAsFactors = F, skip = 6)[,inp_cols]

    colnames(data) <- c("Viability", "Cell.No", "Death", "SampleID")

    data$SampleID <- gsub(" ", "", data$SampleID)

    df <- merge.data.frame(label, data, by = "SampleID", all = T)
    df$plate <- i

    if(sum(is.na(df))>0){
      unlabelled <- paste(df$SampleID[apply(df, 1, FUN= function(x){sum(is.na(x))>0})], collapse=", ")
      print(paste("file",i, "missing labels", unlabelled, collapse = ""))
    }

    return(df)
  }

  if(!is.null(Cell.line)){

    out$Cell.Line <- Cell.line
  }

  return(out)
}

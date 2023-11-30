
######################## IMport from Guava ####################################################################
#' @name  GuavaSmoothy_dir
#' @export GuavaSmoothy_dir
#' @title  Import values from VIA files
#' @usage GuavaSmoothy("assay_file.VIA")
#' @param filename path and filename for target file
#' @param easyfit if true values from easyfit will be used
#' @param label labels generated from import labels to be used

GuavaSmoothy_dir <- function(labels, Folder, easyfit = FALSE){
  require(dplyr)
  require(foreach)

    if(easyfit == TRUE){
      inp_cols <- c(77, 84, 78, 2)
    }else{
      inp_cols <- c(12, 9, 14, 2)
    }

  names(labels) <- paste(names(labels), ".VIA.CSV", sep = "")

  filenames <-names(labels)
  filenames <- filenames[filenames %in% grep(list.files(Folder), pattern = ".VIA.CSV", fixed = T, value = T)]

  out <- foreach(i = filenames, .combine = "rbind")%do%{


  label <- labels[[i]]

  data <- read.csv(paste(Folder, i, sep = "/"), stringsAsFactors = F, skip = 6, check.names = F, encoding = "UTF-8")[,inp_cols]

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

  return(out)
  }

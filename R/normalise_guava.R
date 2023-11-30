#' @name  normalise_guave
#' @export normalise_guave
#' @title  Import values from VIA files
#' @usage GuavaSmoothy("assay_file.VIA")
#' @param x path and filename for target file


normalise_guava <- function(x, dmso = T, remove_unlabelled = T){
  require(foreach)

  if(remove_unlabelled == T){
    x <- x[!(is.na(x$Cell.Line)|is.na(x$Drug)), ]
    }

  cell_lines <-x$Cell.Line[x$Cell.Line!="NA"] %>% unique() %>% paste()

  if(dmso==T){
    filt <- toupper(x$Drug)=="DMSO"
    }else{
    filt <- x$Concentration == 0
  }

  df_filt <- x[filt,] %>% na.omit()


  out <- foreach(i=cell_lines, .combine = "rbind")%do%{

    df <- x[x$Cell.Line==i,]

    df_f <- df_filt[df_filt$Cell.Line==i,]

    mv <- mean(df_f[, "Viability"], na.rm = T)
    mc <- mean(df_f[, "Cell.No"], na.rm = T)

    df[, "Viability"] <- (df[, "Viability"]/mv) * 100
    df[, "Cell.No"] <- (df[, "Cell.No"]/mc)* 100

    return(df)
  }

  return(out)
}

#' @name  Guava_PDAnalysis
#' @title  Guava PharmacoDynamic Analysis
#' @description  Calculated IC50 and AUC values for all metrics for all drug and cell line combinations
#' @usage Guava_PDAnalysis(GuavaSmoothy_object)
#' @param x Normalised GuavaSmoothy object
#' @export Guava_PDAnalysis
#' @return Returns a data.frame containing "AUC_Viability", "AUC_CellNo", "AUC_death", "IC50_Viability", "IC50_CellNo", "IC50_Death", "Drug" and "Cell.Line".


Guava_PDAnalysis <- function(x, .area.type = "Actual"){

  require(PharmacoGx)
  require(foreach)
  require(dplyr)

  cell_lines <- x$Cell.Line %>% unique()
  cell_lines <- cell_lines[cell_lines != "NA"]
  drugs <- x$Drug %>% unique()
  drugs <- drugs[!drugs %in% c("NA", "DMSO")]

  df <- plyr::ddply(
    .data = x,
    .variables = c("Concentration","Drug", "Cell.Line"),
    .fun =  summarize,
    mean_viability = mean(Viability, na.rm=T),
    mean_cell_no = mean(Cell.No, na.rm=T),
    mean_death = mean(death, na.rm = T)
  )


  out <- foreach(i = cell_lines, .combine = "rbind" ) %do%{

    df <- x[x$Cell.Line==i,]

    df <- lapply(drugs, function(x){df[df$Drug %in% c("DMSO", x),]})
    names(df) <- drugs

    aggregate(x, by = list(x$Concentration), FUN = mean)
    df <- lapply(drugs, function(x){
      drug <- x
      x <- df[[x]]
      x <- aggregate.data.frame(x, by = list(x$Concentration), FUN = mean)


      AUC_Viability <-
        PharmacoGx::computeAUC(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Viability,
          viability_as_pct = T,
          area.type = .area.type,
          trunc = F,
          verbose = F
        )
      AUC_Cell.No <-
        PharmacoGx::computeAUC(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Cell.No,
          viability_as_pct = T,
          area.type = .area.type,
          trunc = F,
          verbose = F
        )
      AUC_Death <-
        PharmacoGx::computeAUC(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Death,
          viability_as_pct = T,
          area.type = .area.type,
          trunc = F,
          verbose = F
        )
      IC50_Viability <-
        PharmacoGx::computeIC50(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Viability,
          viability_as_pct = T
        )
      IC50_Cell.No <-
        PharmacoGx::computeIC50(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Cell.No,
          viability_as_pct = T
        )
      IC50_Death <-
        PharmacoGx::computeIC50(
          concentration = x$Concentration,
          conc_as_log = F,
          viability = x$Death,
          viability_as_pct = T
        )

      out <- data.frame(AUC_Viability, AUC_Cell.No, AUC_Death, IC50_Viability,IC50_Cell.No, IC50_Death)
      out$Drug <- drug
      return(out)
    })

    names(df)<- drugs
    df <- bind_rows(df)
    df$Cell.Line <- i

    print(paste(i, "values calculated"))

    return(df)
  }

  rownames(out) <- 1:nrow(out)
  return(out)

}

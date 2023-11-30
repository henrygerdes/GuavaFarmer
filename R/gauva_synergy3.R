#' @title Calculate Guava Synergy Scores
#' @param x Normalise GuavaSmoothy Object
#' @param combinations list of drug combinations for synergy calculation. If NULL all combination treatments in data will be used.
#' @param combo_sep Separator used between drug concentrations and names default is "&"
#' @param as.percentage if true viability, cell fold change and cell death values will be treated as percentages
#'
#' @export Guava_synergy3

Guava_synergy3 <-
  function(x,
           combinations = NULL,
           combo_sep = "&",
           as.percentage = T) {

    if (is.null(combinations)) {
      x$combinations <- grepl(x$Drug, pattern = combo_sep, fixed = T)
    } else{
      x$combinations <- x$Drug %in% combinations
    }

    if(as.percentage){
      x$Viability <- as.numeric(x$Viability)/100
      x$Cell.No <- as.numeric(x$Cell.No)/100
      x$Death <- as.numeric(x$Death)/100

    }else{
      x$Viability <- as.numeric(x$Viability)
      x$Cell.No <- as.numeric(x$Cell.No)

    }

    x$Fold <- 1 - x$Cell.No

    # get list of combinations for analysis - forloop can iterate between them
    combinations <- unique(x$Drug[x$combinations])
    # get list of combinations
    df_combo <- x[x$combinations, ]

    #get list of combinations
    df_combo$drug1 <- stringr::word(df_combo$Drug, sep = combo_sep, start = 1)
    df_combo$drug2 <- stringr::word(df_combo$Drug, sep = combo_sep, start = 2)
    df_combo$conc1 <- stringr::word(df_combo$Concentration, sep = combo_sep, start = 1) %>% as.numeric()
    df_combo$conc2 <- stringr::word(df_combo$Concentration, sep = combo_sep, start = 2) %>% as.numeric()

    #get list of drugs
    drugs <- unique(unlist(df_combo[,c("drug1", "drug2")]))

    df_mono <- x[x$Drug %in% drugs, ]
    df_mono <- plyr::ddply(.data = df_mono, .variables = c("Concentration", "Drug", "Cell.Line"),
                           .fun = summarize,
                           "mean_viability" = mean(as.numeric(Viability), na.rm = T),
                           "mean_cell_no" = mean(as.numeric(Cell.No), na.rm = T),
                           "mean_Death" = mean(as.numeric(Death), na.rm=T),
                           "mean_fold" = mean(as.numeric(Fold), na.rm=T),
    )

    # bliss score

    df_comps <-df_combo[,c("drug1","drug2", "conc1","conc2", "Cell.Line")]

    df_drug_all <- merge.data.frame(x = df_combo,
                                 by.x = c("drug1", "conc1", "Cell.Line"),
                                 y = df_mono,
                                 by.y = c("Drug", "Concentration", "Cell.Line")
    )

    #drug 1 == mean viabilityx
    #drug 2 == mean viabilityy

    df_drug_all <- merge.data.frame(x = df_drug_all,
                                 by.x = c("drug2", "conc2", "Cell.Line"),
                                 y = df_mono,
                                 by.y = c("Drug", "Concentration", "Cell.Line")
    )

    df_out <-  plyr::mutate(df_drug_all,
                          Viability_CI = (Viability / (mean_viability.x  * mean_viability.y)),
                           Cell.No_CI = (Cell.No / (mean_cell_no.x  * mean_cell_no.y)),
                          Mort_Bliss = (Death - (mean_Death.x + mean_Death.y - (mean_Death.x * mean_Death.y))),
                           Fold_bliss = (Fold - (mean_fold.x + mean_fold.y - (mean_fold.x* mean_fold.y)))
                           )
    df_out <-
      df_out[, c(
        "Drug",
        "Cell.Line",
        "Concentration",
        "drug1",
        "drug2",
        "conc1",
        "conc2",
        "Viability_CI",
        "Cell.No_CI",
        "Mort_Bliss",
        "Fold_bliss"
      )]
    return(df_out)
  }


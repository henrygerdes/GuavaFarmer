#' @title Calculate Guava Synergy Scores
#' @param x Normalise GuavaSmoothy Object
#' @param combinations list of drug combinations for synergy calculation. If NULL all combination treatments in data will be used.
#' @param combo_sep Seperater used between drug concentrations and names default is "&"
#'
#' @export Guava_synergy

Guava_synergy <-
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
                           mean_viability = mean(as.numeric(Viability), na.rm = T),
                           mean_cell_no = mean(as.numeric(Cell.No), na.rm = T),
                           mean_Death = mean(as.numeric(Death), na.rm=T)
    )
    df_mono$Concentration <- as.numeric(df_mono$Concentration)

    # bliss score

    df_drug1 <- merge.data.frame(x = df_combo,
                                 by.x = c("drug1", "conc1", "Cell.Line"),
                                 y = df_mono,
                                 by.y = c("Drug", "Concentration", "Cell.Line")
    )

    df_drug2 <- merge.data.frame(x = df_combo,
                                 by.x = c("drug2", "conc2", "Cell.Line"),
                                 y = df_mono,
                                 by.y = c("Drug", "Concentration", "Cell.Line")
    )


    df_combo$Viability_Bliss <-
      as.numeric(df_combo$Viability) - (
        df_drug1$mean_viability + df_drug2$mean_viability - (
          df_drug1$mean_viability  * df_drug2$mean_viability
        )
      )
    df_combo$Cell.No_Bliss <-
      as.numeric(df_combo$Cell.No) - (
        df_drug1$mean_cell_no + df_drug2$mean_cell_no - (
          df_drug1$mean_cell_no  * df_drug2$mean_cell_no
        ))

    df_combo$Viability_CI <-
      df_combo$Viability / (df_drug1$mean_viability * df_drug2$mean_viability)
    df_combo$Cell.No_CI <-
      df_combo$Cell.No / (df_drug1$mean_cell_no * df_drug2$mean_cell_no)

    df_combo$Mort_Bliss <-
      df_combo$Death - (df_drug1$mean_Death + df_drug2$mean_Death - (df_drug1$mean_Death * df_drug2$mean_Death))

    df_combo <- df_combo[, c("Cell.Line",
                             "Drug","drug1",
                             "conc1",
                             "drug2",
                             "conc2",
                             "Concentration",
                             "Viability_Bliss",
                             "Cell.No_Bliss",
                             "Viability_CI",
                             "Cell.No_CI",
                             "Mort_Bliss")]

    return(df_combo)
  }


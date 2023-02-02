###viaplot

plate_check <-
  function(guava_data,
           plate,
           value = "Viability",
           Via_labels = F) {
    require(tidyverse)
    wells <-
      expand.grid("column" = formatC(1:12, width = 2, flag = 0),
                  "row" = letters[1:8])
    wells$SampleID <-
      paste(wells$row, wells$column, sep = "") %>% toupper()
    wells$row <-
      factor(wells$row, levels = c("h", "g", "f", "e", "d", "c", "b", "a"))

    df_graph <- guava_data[guava_data$plate == plate,]
    df_graph  <-
      merge.data.frame(df_graph, wells, by = "SampleID", all = T)
    df_graph$Cell.No <-
      (df_graph$Cell.No / max(df_graph$Cell.No, na.rm = T)) * 100
    df_graph$labs <-
      ifelse((is.na(df_graph$Cell.Line) |
                is.na(df_graph$Drug)) & is.na(df_graph$Viability) == F, "red", NA)


    pp <-
      ggplot(df_graph, aes(
        x = column,
        y = row,
        col = df_graph[, value],
        label = round(df_graph[, value], 0)
      )) +
      geom_point(size = 10, aes(x = column, y = row), col = df_graph$labs) +
      geom_point(size = 8) + theme_bw() +
      labs(col = value)
    if (value == "Viability") {
      pp <- pp + scale_colour_gradient(
        limit = c(0, 100),
        low = "white",
        high = "chartreuse2",
        na.value = "grey80",
        guide = "colourbar",
        aesthetics = "colour"
      ) +
        ggtitle(paste("Plate check of",
                      plate,
                      "Viability (%)"))
    } else if (value == "Cell.No") {
      pp <- pp + scale_colour_gradient(
        limit = c(0, 100),
        low = "white",
        high = "cyan2",
        na.value = "grey80",
        guide = "colourbar",
        aesthetics = "colour"
      ) +
        ggtitle(paste("Plate check of",
                      plate,
                      "Cell Number (%)"))
    }

    if (Via_labels == T) {
      pp <- pp + geom_text(col = "black")

    }

    return(pp)
  }

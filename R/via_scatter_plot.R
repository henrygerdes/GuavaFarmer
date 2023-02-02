plot_via_scatter <- function(guava_data, Cell.Line, Drugs, Metric, Method){


  df_graph <- guava_data[(guava_data$Cell.Line %in% Cell.Line) & (guava_data$Drug %in% c(Drugs, "DMSO")),c("SampleID", "Cell.Line", "Drug", "Concentration", Metric)]

  colnames(df_graph) <- c("SampleID", "Cell.Line", "Drug", "Concentration","Metric")

  guava_calc <- split.data.frame(x = df_graph, f = df_graph$Drug)

  guava_calc <- lapply(guava_calc, FUN=function(x){rbind(x, guava_calc$DMSO)})
  guava_calc <- guava_calc[names(guava_calc)!="DMSO"]

  guava_calc <- foreach(i=Drugs,.combine = "rbind")%do%{
    x <- guava_calc[[i]]
    x <- x[, c("Concentration", "Metric")]
    out <- data.frame(
      "mean" = aggregate.data.frame(x, by = list(x$Concentration), FUN= mean, ),
      "sd" = aggregate.data.frame(x, by = list(x$Concentration), FUN= sd),
      stringsAsFactors = F)
    out <- out[, c( "mean.Concentration", "mean.Metric","sd.Metric")]#
    colnames(out) <- c("Concentration", "Mean", "SD")
    out$log_conc <- log1p(out$Concentration)
    out$metric_norm <- out$Mean/(max(out$Mean)+max(out$SD))
    out$Drug <- i

    return(out)
  }


  max_val <- max(guava_calc$Mean) + max(guava_calc$SD)
  if(max_val <=100){max_val=100}
  pp <-
    ggplot(data = guava_calc, aes(x = Concentration, y=Mean, col = Drug))+
    geom_pointrange(data = guava_calc, aes(x = Concentration, ymin=Mean-SD,
                                           ymax=Mean+SD, col = Drug))+
    scale_x_continuous(trans = "log1p", breaks = unique(guava_calc$Concentration))+
    xlab("Concentration (nM)")+
    ylab(paste(Metric, "(%)"))+
    ylim(c(0,max_val))+
    ggthemes::scale_colour_colorblind()

    theme_classic()

  #make cutstom sigmoidal curves
  if(Method == "nls"){

    df_graph <- foreach(i=Drugs,.combine = "rbind")%do%{
      x <- df_graph[df_graph$Drug ==i, ]
      x$log_conc <- log1p(x$Concentration)
      x$metric_norm <- x$Metric/max(x$Metric)
      return(x)
    }

    conc_pred <- log1p(seq(0,max(guava_calc$Concentration), length.out = 100))

    guava_lines <- foreach(i = Drugs, .combine="rbind")%do%{
      df <- guava_calc[guava_calc$Drug==i,]
      bfl <- nls(data = df, formula = metric_norm ~ SSlogis(log_conc,Asym,xmid,scal))
      pred <- data.frame((exp(conc_pred)-1), (predict(bfl, conc_pred)*(max(df$Mean)+max(df$SD))))
      pred$Drug <- i
      pred$r2 <- sum((residuals(bfl)*100)^2)
      colnames(pred) <- c("Concentration", "Metric_1", "Drug", "r2")
      print(pred)
      return(pred)
    }
     pp <- pp +geom_line(data = guava_lines, aes(x=Concentration, y = Metric_1, col = Drug), size=2)
  }else{
   pp <- pp + geom_smooth(method = Method, size=2, se=F)
  }

  return(pp)
}

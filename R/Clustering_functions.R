
#' @title Make Kmeans Clusters
#'
#' @param x Imported FCS data
#' @param my.seed seed to use for Kmeans clustering
#' @param .iter.max maximum number of kmeas interations
#' @param .nstart kmeans nstart
#' @param .centers number of centers to use for analysis
#' @param my_minpts minimum number of points needed for kmeans to identify a cluster
#'
#' @return return list with raw data and labelled outputs
#' @export make.kmeans.clusters


make.kmeans.clusters <- function(x, my.seed = 123, .iter.max = 1000, .nstart = 50, .centers = 2, my_minpts = NULL){


  plates <- x$file %>% unique() %>% paste()
  set.seed(my.seed)

  out <- foreach(i=plates, .combine = "rbind", .errorhandling = "remove")%do%{
    df <- subset(x, file == i)
    mymod <- kmeans(x = df[,c("Viability_scaled", "FSC_scaled", "Nucleated_cells_scaled")], centers = .centers, nstart = .nstart, iter.max = .iter.max)


    df$cluster <- mymod$cluster
    return(df)

  }
  print("Cluster_identification complete")

  colnames(out)
  df_calc <- plyr::ddply(.data = out,
                         .variables = c("SampleID", "cluster","plate", "file"),
                         .fun = plyr::summarize,
                         "mean_nucleated" = mean(Nucleated_cells_scaled, na.rm=T),
                         "mean_viability" = mean(Viability_scaled, na.rm=T),
                         "mean_FSC" = mean(FSC_scaled, na.rm=T),
                         "SD_nucleated" = sd(Nucleated_cells_scaled, na.rm=T),
                         "SD_viability" = sd(Viability_scaled, na.rm=T),
                         "SD_FSC" = sd(FSC_scaled, na.rm=T),
                         "n_cells" = length(cluster),
                         "ul" = mean(Total.Volume.Acquired..µL., na.rm=T, parallel = T)
  )


  df_clust_calc <- plyr::ddply(.data = out,
                               .variables = c("cluster","plate","file"),
                               .fun = plyr::summarize,
                               "mean_nucleated" = mean(Nucleated_cells_scaled, na.rm=T),
                               "mean_viability" = mean(Viability_scaled, na.rm=T, parallel = T)
  )

  df_clust_calc <- foreach(i = df_clust_calc$file, .combine = "rbind")%do%{
    df_c  <- subset(df_clust_calc, file == i)
    df_c$status <- ifelse(df_c$mean_viability==min(df_c$mean_viability), yes = "alive", no = "dead")
    df_c$nucleated <- ifelse(df_c$mean_nucleated ==min(df_c$mean_nucleated), yes = "DNA_damage", no = "undamaged")
    return(df_c)
  }


  df_calc <- merge.data.frame(df_calc, df_clust_calc[,c("cluster","plate", "file", "status","nucleated")], by  = c("cluster", "file", "plate"))

  df_out <- merge.data.frame(out, df_clust_calc[,c("cluster", "plate","file", "status","nucleated")], by  = c("cluster", "file", "plate"))


  df_calc2 <- reshape2::dcast(data = df_calc, formula = plate+file +SampleID + ul ~ status, value.var = "n_cells", fun.aggregate = mean)
  df_calc2 <-  mutate(df_calc2,
                      Viability = (alive/(alive + dead))*100,
                      Death  = (dead/(alive + dead))*100,
                      Total = (alive + dead),
                      Cell.No = (alive / ul),
                      Cell.No.all = (Total/ul)
  )
  out <- list("output" = df_calc2, "raw" = df_out)

  return(out)

}

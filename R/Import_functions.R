
#' @title Import FCS data from VIA file
#'
#' @param files_folder folder containing .VIA.FCS file
#' @param cores Number of cores to use for import
#'
#' @return Returns a dataframe contanining all data from the 96 well plate
#' @export import.fcs.via
#'
#' @examples
#'
#'
import.fcs.via <- function(files_folder, .cores = 1){

  cl <- parallel::makeCluster(.cores)

  doParallel::registerDoParallel(cores = cl)

  my_files <- list.files(files_folder)

  my_files <-my_files[stringr::str_detect( my_files, stringr::regex(".VIA.FCS$"))]


  all_frames <- expand.grid(my_files, c(1:96))

  i <- 1

  all_dat <- foreach(i = 1:nrow(all_frames), .combine = "rbind", .errorhandling = "remove")%dopar%{

    ting <- flowCore::read.FCS(paste(files_folder, all_frames[i,1], sep = "/"), dataset = all_frames[i,2], truncate_max_range = F)

    out <- data.frame(ting@exprs)
    out$SampleID <- ting@description[["GTI$SAMPLEID"]]
    out$plate <- gsub(x = paste(all_frames[i,1]), pattern = ".VIA.FCS", replacement = "")
    out$file <- ting@description[["FILENAME"]]

    return(out)
  }

  doParallel::stopImplicitCluster(cl)

  plates <- unique(all_dat$plate) %>% paste()
  i <- plates[1]
  out <- foreach(i = plates, .combine = "rbind")%do%{

    flow_rate <- readr::read_csv(paste(files_folder, "/",i, ".VIA.CSV", sep = ""), show_col_types = FALSE,skip = 6, name_repair = "minimal", locale = locale(encoding="latin1"))[,c("Sample ID", "Total Volume Acquired (µL)", "Acquisition Time (s)")] %>% data.frame(check.names = F)
    flow_rate$`Sample ID` <- gsub(flow_rate$`Sample ID`, pattern = " ", replacement = "")
    mymat <- subset(all_dat, plate == i)
    mymat <- merge.data.frame(x = mymat, y = flow_rate, by.x = "SampleID", by.y = "Sample ID", all.x = T)
    mymat[,c("Viability", "FSC", "Nucleated_cells")] <-log1p(mymat[,c("FL1.PL", "FS.PL", "FL2.PL")])
    mymat[,c("Viability_scaled", "FSC_scaled", "Nucleated_cells_scaled")] <- apply(mymat[,c("Viability", "FSC", "Nucleated_cells")],MARGIN = 2, scales::rescale_max, to = c(0,1))

    return(mymat)

  }

  return(out)

}



#' @title Import FCS data from FCS file (*not recommended*)
#'
#' @param files_folder folder containing FCS files
#'
#' @return matrix containing imported FCS data from FCS files
#' @export import.fcs
#'
#' @examples

import.fcs <- function(files_folder){

  my_files <- list.files(files_folder)

  my_files <-my_files[stringr::str_detect( my_files, stringr::regex("[0-9].FCS$"))]

  all_dat <- foreach(i = my_files, .combine = "rbind")%do%{


    ting <- flowCore::read.FCS(paste(files_folder, i, sep = "/"))

    out <- data.frame(ting@exprs)
    sum(out$TIME)
    out$well <- i
    out$SampleID <- ting@description[["GTI$SAMPLEID"]]
    return(out)
  }

  all_dat$well_no <- stringr::str_extract(string = all_dat$well, stringr::regex("[0-9][0-9][0-9][0-9]")) %>% as.numeric()
  all_dat$plate <-  stringr::str_remove(string = all_dat$well, stringr::regex("_[0-9][0-9][0-9][0-9].FCS$"))
  plates <- unique(all_dat$plate) %>% paste()

  out <- foreach(i = plates, .combine = "rbind")%do%{

    flow_rate <- read.csv(paste(files_folder, "/",i, ".VIA.CSV", sep = ""), skip = 6, check.names = F)[,c("Sample ID", "Total Volume Acquired (µL)", "Acquisition Time (s)")]
    flow_rate$`Sample ID` <- gsub(flow_rate$`Sample ID`, pattern = " ", replacement = "")
    mymat <- subset(all_dat, plate == i)
    mymat <- merge.data.frame(x = mymat, y = flow_rate, by.x = "SampleID", by.y = "Sample ID")
    mymat[,c("Viability", "FSC", "Nucleated_cells")] <-log1p(mymat[,c("FL1.PL", "FS.PL", "FL2.PL")])
    mymat[,c("Viability_scaled", "FSC_scaled", "Nucleated_cells_scaled")] <- DRUMLR::MinMaxNormalise(mymat[,c("Viability", "FSC", "Nucleated_cells")], .margin = 2)

    return(mymat)

  }

  return(out)

}

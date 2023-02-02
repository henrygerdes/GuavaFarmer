#' Guava Scatterplot
#'
#' @param x guava_data
#' @param combo if TRUE combinations will be converted for graphing
#' @param drugs list of drugs to analyse if NULL all will be used
#' @param cell_lines list of cell lines to analyse if NULL all will be used
#' @param .ncols number of columns used in output
#' @param line.size size of line used
#' @param point.size size of datapoints used
#' @param .rel.widths relative width ratios between scatterplots and legened
#' @param errorbar.size size of errorbars
#'
#' @return
#' @export guava_scatterplot
#'
#' @examples


guava_scatter_plot <- function(x,
                               combo = T,
                               drugs = NULL,
                               cell_lines = NULL,
                               .ncols = 2,
                               line.size = 2,
                               point.size = 3,
                               .rel.widths = c(0.6, 0.2),
                               errorbar.size = 0.25) {

  if(!is.null(drugs)){
    x <- x[x$Drug %in% c("DMSO",drugs),]
  }

  if(!is.null(cell_lines)){
    x <- x[x$Cell.Line %in% cell_lines,]
  }

  if(combo){
    #Rename concentrations
    new_conc <-
      stringr::word(string = x$Concentration,
                    sep = "&",
                    start = 1) %>% as.numeric()
    new_drug <-
      paste(
        stringr::word(x$Drug, sep = "&", start = 1),
        "+",
        stringr::word(
          string = x$Concentration,
          sep = "&",
          start = 2
        ),
        "nM ",
        stringr::word(
          string = x$Drug,
          sep = "&",
          start = 2
        ),
        sep = ""
      )
    new_drug <-
      gsub(
        x = new_drug,
        pattern = "+NAnM NA",
        replacement = "",
        fixed = T
      )
    x$Drug <- new_drug
    x$Concentration <- new_conc
  }

  #create 0 values for all conditions
  drugs <- unique(x$Drug)
  drugs <- drugs[drugs != "DMSO"]

  x2 <- foreach(i=drugs, .combine = "rbind")%do%{
    out <- x[x$Drug == "DMSO", ]
    out$Drug <- i

    return(out)
  }

  #add 0 values to whole dataset
  x <- rbind(x2, x[x$Drug !="DMSO", ])


  #summarise data
  df <- plyr::ddply(
    .data = x,
    .variables = c("Drug", "Cell.Line", "Concentration"),
    summarize,
    mean_viability = mean(Viability),
    sd_viability = sd(Viability),
    mean_Cell.No = mean(Cell.No),
    sd_Cell.No = sd(Cell.No)
  )

  #plot viability curve
  pp <- ggplot(df, aes(x = as.numeric(Concentration), y=mean_viability, col = Drug))+
    geom_line(stat= "smooth", method = "loess", alpha = 0.5, se = F, size = line.size)+
    geom_errorbar(aes(ymin = (mean_viability - sd_viability),
                      ymax = (mean_viability + sd_viability)),
                  size = errorbar.size, width = 0.25)+
    geom_point(size = point.size)+
    scale_x_continuous(breaks = as.numeric(unique(x$Concentration)),
                       trans = "log1p",
                       limits = c(0, max(x$Concentration)))+
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                       #                   limits = c(-10,120)
    )+
    facet_wrap(~Cell.Line, ncol = .ncols)+
    ylab("Viability(%)")+
    xlab("Concentration(nM)")+
    theme_bw()+
    ggthemes::scale_colour_colorblind()+
    theme(legend.position = "none")

  #plot fold change curve
  pp2 <- ggplot(df, aes(x = as.numeric(Concentration), y=mean_Cell.No, col = Drug))+
    geom_line(stat= "smooth", method = "loess",
              alpha = 0.5, se = F, size = line.size)+
    geom_errorbar(aes(ymin = (mean_Cell.No - sd_Cell.No),
                      ymax = (mean_Cell.No + sd_Cell.No)),
                  size = errorbar.size, width = 0.25)+
    geom_point(size = point.size)+
    ggthemes::scale_colour_colorblind()+
    scale_x_continuous(breaks = as.numeric(unique(x$Concentration)),
                       trans = "log1p",
                       limits = c(0, max(x$Concentration)))+
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                       #                  limits = c(-10,120)
    )+
    facet_wrap(~Cell.Line, ncol = .ncols)+
    ylab("Cell Number Fold Change(%)")+
    xlab("Concentration(nM)")+
    theme_bw()+
    theme(legend.position = "none")

  #get colour drug legend
  pp3 <- cowplot::get_legend(ggplot(df, aes(x = as.numeric(Concentration), y=mean_Cell.No, col = Drug))+ ggthemes::scale_colour_colorblind() +geom_point()+ theme(legend.position = "right"))

  #plot viability plots
  col_1 <- cowplot::plot_grid(plotlist = list(pp, pp2),
                              ncol = 1)

  #plot viability plots and legend
  out <- cowplot::plot_grid(col_1, pp3,
                            ncol = 2,
                            rel_widths = .rel.widths)

  return(out)

}


#' Scatter Plot Wizard
#'
#' @param x guava data
#' @param .combo if TRUE plots will be treated as combination treatments
#'
#' @return
#' @export Scatter.plot.wizard
#'
#' @examples
#'
#'
Scatter.plot.wizard <- function(x, .combo = F){

  cell_lines <- x$Cell.Line %>% unique()
  drugs <- x$Drug %>% unique()
  drugs <- drugs[drugs!="DMSO"]

  ui <- shiny::fillPage(
    shiny::sidebarPanel(
      shiny::checkboxGroupInput(
        inputId = "Cell.Line",
        label = "Cell line",
        choices = cell_lines,
        selected = unlist(cell_lines)
      ),
      shiny::checkboxGroupInput(
        inputId = "Drug",
        label = "drugs",
        choices = drugs,
        selected = unlist(drugs)
      ),

      shiny::numericInput(
        inputId = "ncols",
        label = "Number of columns",
        min = 1,
        max = 12,
        step = 1,
        value = 2
      )
      ,

      shiny::numericInput(
        inputId = "errorbar_size",
        label = "Errorbar size",
        min = 0,
        max = 4,
        step = 0.1,
        value = 0.25
      )
      ,
      shiny::numericInput(
        inputId = "line_size",
        label = "Number of columns",
        min = 0,
        max = 5,
        step = 0.1,
        value = 2
      )
      ,shiny::actionButton(inputId = "save_plot",
                           label = "Save Plot"),

      shiny::numericInput(inputId = "plotheight", label = "svg Height (cm)", value = 28.7, min = 50, max = 1000, step = 50),
      shiny::numericInput(inputId = "plotwidth", label = "svg Width (cm)", value = 20, min = 50, max = 1000, step = 50),
      shiny::numericInput(inputId = "plotdpi", label = "dpi", value = 320, min = 72, max = 320, step = 10),
      shiny::numericInput(inputId = "plotscale", label = "
Multiplicative scaling factor", value = 1, min = 0, max = 5, step = 0.1),
      shiny::textInput(inputId = "save_name", label = "Save as", value = "Scatterplot", placeholder = "type filename here"),
    ),
    shiny::mainPanel(shiny::plotOutput("graph"))



  )


  server <- function(input, output) {

    output$graph <- shiny::renderPlot({

      guava_scatter_plot(x = x,
                         combo = .combo,
                         drugs = paste(input$Drug),
                         cell_lines = paste(input$Cell.Line),
                         .ncols = input$ncols,
                         line.size = input$line_size,
                         .rel.widths = c(6,2),
                         errorbar.size = input$errorbar_size)
    }
    )



    shiny::observeEvent(input$save_plot, {


      pp <- guava_scatter_plot(x = x,
                               combo = .combo,
                               drugs = paste(input$Drug),
                               cell_lines = paste(input$Cell.Line),
                               .ncols = input$ncols,
                               line.size = input$line_size,
                               .rel.widths = c(6,2),
                               errorbar.size = input$errorbar_size)

      ggsave(
        plot = pp,
        filename = paste(input$save_name, "svg", sep = "."),
        device = "svg",
        width = input$plotwidth,
        height = input$plotheight,
        units = "cm",
        dpi = input$plotdpi,
        scale = input$plotscale
      )

      shell.exec(paste(input$save_name, "svg", sep = "."))

    })

  }

  shiny::shinyApp(ui, server)

}

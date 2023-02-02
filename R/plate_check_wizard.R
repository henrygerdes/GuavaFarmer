plate_check_wizard <- function(guava_data){
require(shiny)
plates <- as.factor(guava_data$plate) %>% levels()


ui <- shiny::fluidPage(
  shiny::sidebarPanel(
    shiny::selectInput(
      inputId = "plate",
      label = "Plate",
      choices = plates,
      selected = plates[1]
    ),
    shiny::checkboxInput(
      inputId = "Via_labels",
      label = "Show Measurements",
      value = F
    )
   ),
  mainPanel(plotOutput("check1"),
            plotOutput("check2"),
            verbatimTextOutput("info"))

)

server <- function(input, output) {

  output$check1 <- renderPlot({plate_check(guava_data = guava_data,
                              plate = input$plate,
                              value = "Viability",
                              Via_labels = input$Via_labels)})

  output$check2 <- renderPlot({plate_check(guava_data = guava_data,
                                           plate = input$plate,
                                           value = "Cell.No",
                                           Via_labels = input$Via_labels)})



  output$info <- renderText({
    "Unlabelled readings are shown in red"
  })
}
shinyApp(ui, server)

}


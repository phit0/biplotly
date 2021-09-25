library(shiny)

ui <- fluidPage(
  checkboxGroupInput(inputId = "pcs", label = "Components",
              choices = c(1, 2, 3, 4, 5, 6), selected = c(1,2)),
  sliderInput(inputId = "arr_scale", label = "scaling factor for arrows",
              min = 0, max = 50, step = 1,value = 1, round = TRUE),
  
  plotly::plotlyOutput(outputId = "triplot"),
  #textOutput(outputId = "debugtext")
)
data("big5")

server <- function(input, output, session) {
  output$triplot <- plotly::renderPlotly({
    triplotly::triplotly(data = big5[, c(4, 8:ncol(big5))],
                         color = "gender",
                         components = as.numeric(input$pcs),
                         alpha = 1,
                         arr.scale = input$arr_scale)
  })
  #output$debugtext <- renderPrint({as.numeric(input$pcs)})
}

shinyApp(ui, server)


# TODO: 
# make app faster by calculating svd only once and storing the PC data
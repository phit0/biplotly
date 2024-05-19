library(shiny)
library(shinyWidgets)
devtools::load_all("../")
library(dplyr)
#devtools::install_github("https://github.com/phit0/triplotly")
data("big5")


ui <- fluidPage(
  
  titlePanel("Triplotly - interactive PCA"),
  "Shiny dashboard for the R-package triplotly 
  (https://github.com/phit0/triplotly)",
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 9,
          fileInput(inputId = "data", label = "Upload *.csv file",
                    placeholder = "big5_data", accept = ".csv"),
        ),
        column(
          width = 1,
          uiOutput("uploadInfo")
        )
      ),
      dropMenu(
        tag = actionButton("rmMenu", label = "Remove Variables"),
        uiOutput("rmFctr"),
        actionButton(inputId = "ok", label = "OK")
      ),
      
      uiOutput("group"),
      uiOutput("pcs"),
      
      sliderInput(inputId = "arr_scale", label = "scaling factor for arrows",
                  min = 0, max = 50, step = 1,value = 10, round = TRUE),
      # sliderInput(inputId = "alpha", label = "alpha",
      #             min = 0, max = 1, step = 0.02, value = 1, round = TRUE),
      sliderInput(inputId = "opacity", label = "transparency",
                  min = 0, max = 1, step = 0.1, value = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("triplot",
                 plotly::plotlyOutput(outputId = "triplot",
                                      width = "100%",
                                      height = "100%"), fill = FALSE),
        tabPanel("Comp. variances",
                 plotly::plotlyOutput(outputId = "varplot",
                                      width = "90%"))
      )
    )
  )
)

#svd_tbl$debug("initialize")
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$data, ignoreNULL = F, {
    
    upload <- tryUpload(input$data)
    rv$data <- upload$data
    rv$start_group <- upload$start_group
    
    # Intit svd object
    rv$svdtbl <- triplotly::svd_tbl$new(
      rv$data,
      factors = upload$factors
    )
    rv$svdtbl$msg <- append(rv$svdtbl$msg, upload$msg)
    rv$uploadsuccess <- upload$success
    
    output$uploadInfo <- renderUI({
      actionButton(
        inputId = "uploadInfo",
        label = icon("info"),
        class = ifelse(upload$success, "btn-success", "btn-danger")
      )
    })
    
    output$group <- renderUI({
      selectInput("group", "grouping variable",
                  selected = rv$svdtbl$factors[1],
                  choices = rv$svdtbl$factors)
    })
    
    output$pcs <- renderUI({
      isolate({
        # reduce number of pcs to select
        pc0.9 <- min(which(cumsum((rv$svdtbl$svd_obj$d) /
                                    sum((rv$svdtbl$svd_obj$d))) > 0.9))
      })
      selectInput(inputId = "pcs", label = "Select 2 or 3 components",
                  multiple = TRUE, choices = 1:pc0.9, selected = c(1, 2, 3))
    })
    
    output$rmFctr <- renderUI({
      shinyWidgets::multiInput("rmFctr", "Remove Factor Variables from PCA",
                               selected = rv$svdtbl$nonFactors,
                               choiceNames = colnames(rv$svdtbl$data),
                               choiceValues = colnames(rv$svdtbl$data),
                               options = list(
                                 non_selected_header = "Factors",
                                 selected_header = "Numbers"
                               )
      )
    })
    
    rv$svdtbl$doSVD()
    rv$svdtbl$calcBi_df(
      components = c(1, 2, 3),
      group_by = rv$start_group,
      alpha = 1
    )
    # df for variance plot
    rv$df <- data.frame(comp = 1:length(rv$svdtbl$pcvar),
                        variance = rv$svdtbl$pcvar,
                        p_var = rv$svdtbl$ppcvar,
                        cp_var = cumsum(rv$svdtbl$ppcvar))
    
  })
  
  observeEvent(input$uploadInfo, ignoreInit = TRUE, {
    
    showModal(modalDialog(
      title = ifelse(rv$uploadsuccess, 
                     "Upload successful: ", 
                     "Upload failed: "), 
      lapply(rv$svdtbl$msg, function(x) {
        renderPrint(x)
      })
    ))
  })
  
  observeEvent(req(input$ok, input$rmFctr), ignoreNULL = FALSE, {
    
    rv$svdtbl$nonFactors <- input$rmFctr
    
    rv$svdtbl$factors <- rv$svdtbl$data %>%
      select(-all_of(input$rmFctr)) %>% 
      colnames()
  })
  
  observeEvent(input$ok, ignoreNULL = TRUE, {
    hideDropMenu(id = "rmMenu_dropmenu")
    
    output$group <- renderUI({
      selectInput("group", "grouping variable",
                  selected = input$group,
                  choices = rv$svdtbl$factors)
    })
  })
  
  observeEvent(
    c(input$pcs, input$group, input$ok),
    ignoreInit = TRUE, {
      rv$svdtbl$data_sanity(rv$data)
      rv$svdtbl$doSVD()
      rv$svdtbl$calcBi_df(as.integer(input$pcs), input$group, 1)
      rv$df <- data.frame(comp = 1:length(rv$svdtbl$pcvar),
                          variance = rv$svdtbl$pcvar,
                          p_var = rv$svdtbl$ppcvar,
                          cp_var = cumsum(rv$svdtbl$ppcvar)) 
      
      output$triplot <- plotly::renderPlotly({
        rv$svdtbl$plot(
          arr.scale = input$arr_scale,
          opacity = input$opacity
          ) 
      })
    })
  
  output$varplot <- plotly::renderPlotly({
    plotly::plot_ly(rv$df) %>%
      plotly::add_bars(x=~comp, y=~variance,
                       text=~p_var,
                       marker = list(color = "grey"),
                       hovertemplate = "%{text} %") %>%
      plotly::add_trace(x =~comp, y=~cp_var, yaxis = "y2",
                        mode="lines+markers",
                        type="scatter",
                        text = ~cp_var,
                        hoverinfo = "text",
                        line = list(color = "blue"),
                        marker = list(color = "blue")) %>%
      plotly::layout(yaxis2 = list(tickfont = list(color = "blue"),
                                   overlaying = "y",
                                   side = "right",
                                   title = "cumulative variance [%]"),
                     showlegend=FALSE,
                     margin = list(r = 80))
  })
}

shinyApp(ui, server)
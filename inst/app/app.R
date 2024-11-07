# rsconnect::deployApp(appDir = "inst/app", appName = "triplotly")
library(shiny)
library(shinyWidgets)
library(dplyr)
if(!require(triplotly)) {
remotes::install_github(repo = "phit0/triplotly", dependencies = TRUE,
                        upgrade = "never")
}
# devtools::load_all("../")
# library(triplotly)
#TODO: Fixed vs hover label switch
#TODO: Info box with nice markdown docu explaining all the Math
ui <- fluidPage(
  
  titlePanel("Triplotly - interactive PCA"),
  "Shiny dashboard for the R-package triplotly 
  (https://github.com/phit0/triplotly)",
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 5,
          fileInput(inputId = "data", label = "Upload *.csv file",
                    placeholder = "iris", accept = ".csv"),
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
      uiOutput("pcsError"),      
      dropMenu(
        tag = actionButton("figAdj", label = "figure adjustments"),
        sliderInput(inputId = "arr_scale", label = "scaling factor for arrows",
                    min = 0.0001, max = 0.1, step = 0.0001, value = 0.01),
        # sliderInput(inputId = "alpha", label = "alpha",
        #             min = 0, max = 1, step = 0.02, value = 1, round = TRUE),
        sliderInput(inputId = "size", label = "marker size",
                    min = 0.5, max = 15, step = 1, value = 2),
        sliderInput(inputId = "opacity", label = "transparency",
                    min = 0, max = 1, step = 0.1, value = 1),
        switchInput("showLabels", "", value = FALSE,
                    onLabel = "hide Labels", 
                    offLabel = "show Labels")
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("triplot",
                 plotly::plotlyOutput(outputId = "triplot",
                                      width = "900px", 
                                      height ="750px",
                                      fill = FALSE)),
        tabPanel("Comp. variances",
                 plotly::plotlyOutput(outputId = "varplot",
                                      width = "90%"))
      )
    )
  )
)

#trp$debug("initialize")
max_pcs <- 3
min_pcs <- 2

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  
  # data initialization -----------------------------------------------------
  observeEvent(input$data, ignoreNULL = F, ignoreInit = F, {
    
    upload <- triplotly:::tryUpload(input$data)
    rv$data <- upload$data
    rv$start_group <- upload$start_group
    
    # Intit svd object
    rv$trp_obj <- triplotly::TRP$new(
      rv$data,
      factors = upload$factors
    )
    rv$msg <- append(rv$trp_obj$msg, upload$msg)
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
                  selected = rv$start_group,
                  choices = rv$trp_obj$factors)
    })
    
    output$rmFctr <- renderUI({
      shinyWidgets::multiInput("rmFctr", "Remove Factor Variables from PCA",
                               selected = rv$trp_obj$nonFactors,
                               choiceNames = colnames(rv$trp_obj$data),
                               choiceValues = colnames(rv$trp_obj$data),
                               options = list(
                                 non_selected_header = "Factors",
                                 selected_header = "Numbers"
                               )
      )
    })
    
    rv$trp_obj$doSVD()
    rv$trp_obj$calcBi_df(
      components = c(1, 2, 3),
      group_by = rv$start_group,
      alpha = 0 #FIXME: Alpha 1 or 0??
    )
    # df for variance plot
    rv$df <- data.frame(comp = 1:length(rv$trp_obj$pcvar),
                        Variance = rv$trp_obj$pcvar,
                        p_var = rv$trp_obj$ppcvar,
                        cp_var = cumsum(rv$trp_obj$ppcvar))
    
  })
  
  # pc selection ------------------------------------------------------------
  
  observe({
    output$pcs <- renderUI({
      checkboxGroupInput(
        inputId = "pcs",
        label = "select Components",
        choices = 1:min(length(rv$trp_obj$pcvar), 6),
        inline = TRUE,
        selected = c(1, 2, 3))
    })
  })
  observe({
    output$pcsError <- renderText(rv$pcsError)
  })
  
  
  observeEvent(input$uploadInfo, ignoreInit = FALSE, {
    
    showModal(modalDialog(
      title = ifelse(rv$uploadsuccess,
                     "Upload successful: ", 
                     "Upload failed: "), 
      lapply(rv$msg, function(x) {
        renderPrint(triplotly:::prettylog(x))
      })
    ))
  })
  
  observeEvent(req(input$ok, input$rmFctr), ignoreNULL = FALSE, {
    
    rv$trp_obj$nonFactors <- input$rmFctr
    
    rv$trp_obj$factors <- rv$trp_obj$data %>%
      select(-all_of(input$rmFctr)) %>% 
      colnames()
  })
  
  observeEvent(input$ok, ignoreNULL = TRUE, {
    hideDropMenu(id = "rmMenu_dropmenu")
    
    output$group <- renderUI({
      selectInput("group", "grouping variable",
                  selected = input$group,
                  choices = rv$trp_obj$factors)
    })
  })
  
  observeEvent(
    c(input$pcs, input$group, input$ok, input$showLabels),
    ignoreInit = TRUE, {
      # rv$trp_obj$data_sanity(rv$data)
      rv$trp_obj$doSVD()
      tryCatch({
        rv$trp_obj$calcBi_df(
          as.integer(input$pcs),
          input$group,
          alpha = 0)
        rv$pcsError <- ""
      },
      error = function(e) { rv$pcsError <- e$message }
      )
      rv$df <- data.frame(comp = 1:length(rv$trp_obj$pcvar),
                          Variance = rv$trp_obj$pcvar,
                          p_var = rv$trp_obj$ppcvar,
                          cp_var = cumsum(rv$trp_obj$ppcvar)) 
      
      output$triplot <- plotly::renderPlotly(
        {
          rv$trp_obj$plot(
            arr.scale = input$arr_scale,
            opacity = input$opacity,
            size = input$size,
            showLabels = input$showLabels
          )
        })
    })
  
  output$varplot <- plotly::renderPlotly({
    plotly::plot_ly(rv$df) %>%
      plotly::add_bars(x=~comp, y=~Variance,
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
                                   title = "Cumulative Variance [%]"),
                     showlegend=FALSE,
                     margin = list(r = 80))
  })
}

shinyApp(ui, server)
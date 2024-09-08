# rsconnect::deployApp(appDir = "inst/app", appName = "triplotly")
library(shiny)
library(shinyWidgets)
library(dplyr)
# remotes::install_github(repo = "phit0/triplotly", dependencies = TRUE,
#                         upgrade = "never")
# devtools::load_all("../")
library(triplotly)
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
      
      dropMenu(
        tag = actionButton("figAdj", label = "figure adjustments"),
        sliderInput(inputId = "arr_scale", label = "scaling factor for arrows",
                    min = 0, max = 50, step = 1,value = 35, round = TRUE),
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
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$data, ignoreNULL = F, ignoreInit = F, {
    
    upload <- triplotly:::tryUpload(input$data)
    rv$data <- upload$data
    rv$start_group <- upload$start_group
    
    # Intit svd object
    rv$trp_obj <- triplotly::trp$new(
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
    
    output$pcs <- renderUI({
      isolate({
        # reduce number of pcs to select
        pc0.9 <- min(which(cumsum((rv$trp_obj$svd_obj$d) /
                                    sum((rv$trp_obj$svd_obj$d))) > 0.9))
      })
      selectInput(inputId = "pcs", label = "Select 2 or 3 components",
                  multiple = TRUE, choices = 1:pc0.9, selected = c(1, 2, 3))
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
      alpha = 1
    )
    # df for variance plot
    rv$df <- data.frame(comp = 1:length(rv$trp_obj$pcvar),
                        Variance = rv$trp_obj$pcvar,
                        p_var = rv$trp_obj$ppcvar,
                        cp_var = cumsum(rv$trp_obj$ppcvar))
    
  })
  
  observeEvent(input$uploadInfo, ignoreInit = F, {
    
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
      rv$trp_obj$calcBi_df(as.integer(input$pcs), input$group, 1)
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
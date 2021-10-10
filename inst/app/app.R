library(shiny)
devtools::load_all(".")
data("big5")

ui <- fluidPage(
  fileInput(inputId = "data", label = "Upload *.csv file",
            placeholder = "big5_data", accept = ".csv"),
  
  textInput("factors",
            "Columns not to be included in the svd (numeric, e.g. 1:4)", 
            placeholder = "1:5"),
  actionButton(inputId = "ok", label = "OK"),
  uiOutput("group"),
  uiOutput("pcs"),
  
  sliderInput(inputId = "arr_scale", label = "scaling factor for arrows",
              min = 0, max = 50, step = 1,value = 10, round = TRUE),
  sliderInput(inputId = "alpha", label = "alpha",
              min = 0, max = 1, step = 0.02, value = 1, round = TRUE),
  
  
  plotly::plotlyOutput(outputId = "triplot")
)

#svd_tbl$debug("initialize")
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  check_input <- function(fctr, dat) {
    
    if (fctr == "" & is.null(dat)) {
      rv$factors <- 1:5
      rv$data <- big5
    } else if (fctr == "" & !is.null(dat)) {
      rv$data <- readr::read_csv(dat$datapath)
      rv$factors <- which(c(colnames(rv$data)) %in%
                            colnames(dplyr::select_if(rv$data, is.character)))
    } else if (fctr != "" & is.null(dat)) {
      rv$factors <- eval(parse(text = fctr))
      rv$data <- big5
    } else if (fctr != "" & !is.null(dat)) {
      rv$data <- readr::read_csv(dat$datapath)
      rv$factors <- eval(parse(text = fctr))
    }
    # rv$group_opts <- colnames(rv$data[which(apply(
    #   rv$data[rv$factors], 2, function(x) length(unique(x))) <= 8)])
    rv$data <- rv$data %>% dplyr::mutate_at(rv$factors, factor)
    rv$group_opts <- colnames(rv$data[rv$factors])
  }
  # debug(check_input)
  
  observeEvent(input$ok, ignoreNULL = FALSE, {
    isolate({
      check_input(input$factors, input$data)
    })
    output$group <- renderUI({
      
      selectInput("group", "grouping variable", selected = rv$group_opts[1],
                  choices = rv$group_opts)
    })
        })
  
# svd_tbl$debug("initialize")  
  # recalculate everything if data is changed
  observeEvent(input$ok, ignoreNULL = FALSE, {
      isolate({
      check_input(input$factors, input$data)
        
      rv$svdtbl <- triplotly::svd_tbl$new(rv$data,
                             group_by = rv$group_opts[1],
                             factors = rv$factors, 
                             components = c(1, 2),
                             alpha = input$alpha)
      #message("number1")
      })
    output$pcs <- renderUI({
      isolate({
      # reduce number of pcs to select
      pc0.9 <- min(which(cumsum((rv$svdtbl$svd_obj$d) /
                                  sum((rv$svdtbl$svd_obj$d))) > 0.9))
      })
      selectInput(inputId = "pcs", label = "Select 2 or 3 components",
                  multiple = TRUE, choices = 1:pc0.9, selected = c(1,2))
    })
  }) 
  

  
  observeEvent(req(input$pcs, input$group, input$alpha),
               ignoreInit = TRUE, {
    
    isolate({
     # message("number3")
    rv$svdtbl$calcBi_df(components = as.numeric(input$pcs),
                        group_by = input$group,
                        alpha = input$alpha)
      
    })
    })
  
  observeEvent(req(input$pcs, input$group, input$alpha, input$arr_scale), {
      
    output$triplot <- plotly::renderPlotly({
        rv$svdtbl$plot(arr.scale = input$arr_scale)
      }) %>% 
      bindCache(input$pcs, input$group, input$alpha, input$arr_scale)
       #uu message("number2")
  }
  ) 
  
}

shinyApp(ui, server)
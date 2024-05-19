tryUpload <- function(input_object) {
  
  msg <-  list()
  success <- TRUE
  tidy_message <- list(
    "Reminder: Please provide a csv with", 
    "\".\" decimals and \",\" separators in a",
    "tidy format (https://doi.org/10.18637/jss.v059.i10)")
  if (is.null(input_object)) {
    # example data big5
    data <- big5
    msg <- tidy_message
  } else {
    
    # uploaded custom data
    tryCatch({
      data <- read.csv(
        input_object$datapath,
        header = TRUE, 
        sep = ",",
        row.names = NULL
      ) %>% 
        as_tibble()
      
      if(ncol(data) < 5) {
        # Message for e.g. wrong separator
        msg <- append(
          msg, 
          tidy_message
        )
        success <- FALSE
      }
      
    }, error = function(e) {
      success <- FALSE
      msg <- append(
        msg, 
        tidy_message
      )
    })
  } 
  
  if (!success){
    data <- big5
  }
  factors <- data %>% 
    select(where(function(x) is.logical(x) | is.character(x) | is.factor(x))) %>% 
    colnames
  
  data <- data %>% dplyr::mutate_at(factors, factor)
  
  start_group <- sample(
    data %>% 
      select(all_of(factors)) %>% 
      summarise(across(everything(), ~length(unique(.x)))) %>% 
      select(where(function(x) x < 10)) %>%
      colnames(),
    1)
  
  return(list(
    data = data,
    factors = factors,
    msg = msg,
    success = success,
    start_group = start_group
  ))
}
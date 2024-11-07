tryUpload <- function(input_object) {
  
  msg <-  list()
  success <- TRUE
  tidy_message <- list(
    "Upload your own Data: Please provide a csv with \".\" decimals \n
    and \",\" separators in a tidy format. (https://doi.org/10.18637/jss.v059.i10) \n
    For PCA rows with empty values and constant columns will be dropped.")
  if (is.null(input_object)) {
    # # example data big5
    # data <- big5
    # msg <- append("Demo Data: Answers to 50 lickert rated statements from 19719 participants.\n
    #               Source:  https://openpsychometrics.org/_rawdata",
    #               tidy_message)
    # Example data: Iris 
    data <- datasets::iris
    msg <- append(
      "The Example Dataset `Iris` Taken from the `datasets` package: \n
      \t \"This famous (Fisher's or Anderson's) iris data set gives the measurements \n
      in centimeters of the variables sepal length and width and petal length and width,\n
      respectively, for 50 flowers from each of 3 species of iris.\n
      The species are Iris setosa, versicolor, and virginica.\"
      
      Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.
      The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.
      ", 
      tidy_message
    )
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
    data <- datasets::iris
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

prettylog <- function(x) {
  if ("character" %in% class(x)) {
    return(cat(x))
  } else {
    return(x)
  }
}

pretty_msg <- function(msgs) {
  for (x in msgs) {
    if (is.data.frame(x)) {
      print(x)
    } else {
      message(x)
    }
  }
}
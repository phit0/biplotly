#' Triplotly constructor
#'
#' @export 
trp <- R6::R6Class(
  "trp",
  private = list(
    data_sanity = function(data_raw) {
      
      # sanity checks on the data set
      msg <- list()
      # X <- data_raw %>% dplyr::select(!dplyr::all_of(self$factors))
      
      na_summary <- data_raw %>%
        apply(2, function(x) sum(is.na(x)) * 100 / length(x)) 
      
      # Empty columns
      if(max(na_summary) == 100) {
        msg <- append(
          msg, 
          paste("Column(s)", paste0(names(na_summary)[na_summary == 100], collapse = "\n"),
                "are empty and will be removed")
        )
        # message(msg[1])
        data_raw <- data_raw %>%
          dplyr::select(which(na_summary < 100))
      }
      
      assertthat::assert_that(
        all(
          self$factors %in% colnames(data_raw)
        ), 
        msg = paste("The factors",
                    self$factors[which(!self$factors %in% colnames(data_raw))],
                    "are either empty or do not match the data header..."))
      # Rows with NAs
      data <- data_raw %>%
        tidyr::drop_na(!all_of(self$factors))
      if(nrow(data) < nrow(data_raw)) {
        # Message for dropping rows
        msg <- append(
          msg, 
          list(
            paste0("We dropped ", nrow(data_raw) - nrow(data), " rows \n
            The following columns contain most missing values:", sep = "\n"),
            dplyr::tibble(
              col = names(na_summary),
              pct_missing = na_summary
            ) %>% 
              dplyr::arrange(dplyr::desc(pct_missing)) %>%
              head()
          )
        )
      }
      # Constant columns
      X <- data %>% 
        dplyr::select(!dplyr::all_of(self$factors)) %>% 
        dplyr::mutate(across(dplyr::everything(), as.numeric))
      constcols <- apply(X, 2, function(x) var(x) < 1e-17)
      if (any(constcols)) {
        msg <- append(
          msg,
          paste("Column(s)", paste0(names(X)[constcols], collapse = "\n"),
                "are constant and will be removed", sep = "\n")
        )
        X <- X[, !constcols]
      }
      
      # add data
      self$data <- data
      # add X
      self$X <- X
      self$nonFactors <- colnames(X)
      self$msg <- msg
      
      pretty_msg(msg)
      
      invisible(self)
    }
  ),
  public = list(
    svd_obj = NULL,
    pcvar = NULL,
    ppcvar = NULL,
    bi_df = NULL,
    data = NULL,
    X = NULL,
    msg = NULL,
    GH = NULL,
    pcs = NULL,
    factors = NULL,
    nonFactors = NULL,
    group_by = NULL,
    alpha = NULL,
    
#' init trp object
#'
#' @param data A dataframe or tibble to perform PCA 
#' @param factors The columns of the dataframe that should be excluded from PCA, 
#' either as integer or string.
#'
#' @return An object of class "trp" with different functions to explore the data
#' via PCA tri- or biplots.
#' 
#' @export
#'
#' @examples
#' library(triplotly)
#' data(iris)
#' pca1 <- trp$new(iris, factor = "Species")
#' 
    initialize = function(data, factors) {
      
      # assertthat::assert_that(all(factors %in% colnames(data)))
      if (is.numeric(factors)) {
        self$factors <- colnames(data)[factors]
      } else {
        self$factors <- factors
      }
      # debug(self$data_sanity)
      private$data_sanity(data)
      # self$doSVD(data)
      # self$calcBi_df(components, group_by, alpha)
    },
    # # only update group_by
    # change_col = function(group_by) {
    #   self$group_by <- group_by
    #   self$bi_df <- cbind(self$data[group_by], self$GH)
    #   invisible(self)
    # },
#' Title
#'
#' @return
#' @export
#'
#' @examples
    doSVD = function() {
      
      # scale data & calculate single value decomposition
      Z <- scale(self$X, center = TRUE, scale = TRUE)
      self$svd_obj <- svd(Z)
      # save pc variance
      self$pcvar <- (self$svd_obj$d/sqrt(nrow(self$svd_obj$u) - 1))^2
      self$ppcvar <- round(
        self$pcvar / sum(self$pcvar),
        digits = 4
      ) * 100
      invisible(self)
    },
#' Title
#'
#' @param components 
#' @param group_by 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
    calcBi_df = function(components, group_by, alpha) {
      self$pcs <- components
      self$group_by <- group_by
      self$alpha <- alpha
      col <- self$data[group_by]
      A <- self$svd_obj$v
      l <- self$svd_obj$d
      U <- self$svd_obj$u
      # X <- U%*%diag(l)%*%t(A)
      
      # G = ULambda^alpha
      G <- as.data.frame(U[, components]%*%diag(l[components]^alpha))
      colnames(G) <- paste0("G", components)
      
      # H' = Lambda^(1-alpha)A'
      H <- as.data.frame(t(diag(l[components]^(1 - alpha)) %*% 
                             t(A[, components])))
      colnames(H) <- paste0("H", components)
      H <- data.frame(H, variable = colnames(self$X)) #column for variable names
      
      # Matrix of NAs
      na_mat <- matrix(rep(NA, (nrow(G) - nrow(H))* ncol(H)),
                       ncol = ncol(H))
      colnames(na_mat) <- colnames(H)
      
      H <- rbind(H, na_mat)
      # save
      self$GH <- cbind(G, H)
      # join
      self$bi_df <- cbind(col, G, H)
      # message(colnames(self$bi_df))
      invisible(self)
    },
#' Title
#'
#' @param arr.scale 
#' @param scale.pc 
#' @param colorPalette 
#' @param opacity 
#' @param size 
#' @param showLabels 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
    plot = function(arr.scale = 1,
                    scale.pc = FALSE,
                    colorPalette = "RdYlBu",
                    opacity = 1,
                    size = 1,
                    showLabels = FALSE,
                    title = "") {
      invisible(
        triplotly(self, self$factors, self$group_by, self$pcs, self$alpha,
                  arr.scale = arr.scale,
                  scale.pc = scale.pc,
                  colorPalette = colorPalette,
                  size = size, 
                  showLabels = showLabels,
                  opacity = opacity,
                  title = title)
      )
    }
    
  ))

#' PCA bi- and triplots
#' 
#' @param data 
#' @param factors Wich columns of the provided data are used as factors. 
#' They will not be considered in the PCA but may be used as an argument to
#'  `group_by` by the user. 
#' @param group_by Column of your `data` for the color group of the 
#' markers (dots).
#' @param components Which components to display. 
#' A vector of up to three values are possible.
#' @param alpha 
#' @param title Optional title fot the plot.
#' @param arr.scale Scaling factor for the arrows.
#' @param scale.pc 
#' @param colorPalette The `colors` argument passed to [plotly::plot_ly()] i.e. a
#' colorbrewer.org palette name (default is "RdYlBu").  
#' @param opacity 
#'
#' @return a plotly figure object that can be rendered with [base::print()]
#' @export 
#'
#' @examples 
#' library(triplotly)
#' data("iris")
#' # 2d
#' d2 <- triplotly(iris, factors = "Species", group_by = "Species",
#'                 components = c(1, 2), arr.scale = 0.02, size = 3)
#' \dontrun{
#'   print(d2)
#' }
#' 
#' # 3d
#' d3 <- triplotly(data = iris, factors = "Species", group_by = "Species", 
#'                 components = c(1, 2, 3), arr.scale = 0.01, size = 3)
#' \dontrun{
#'   print(d3)
#' }
#' library(MASS)
#' data("birthwt")
#' ?birthwt
#' head(birthwt)
#' birthwt$low <- as.factor(birthwt$low)
#' colnames(birthwt) <- c(
#'   "lower than 2.5 kg",
#'   "mother's age in years.",
#'   "mother's weight in pounds at last menstrual period.",
#'   "mother's race (1 = white, 2 = black, 3 = other).",
#'   "smoking status during pregnancy.",
#'   "number of previous premature labours.",
#'   "history of hypertension.",
#'   "presence of uterine irritability.",
#'   "number of physician visits during the first trimester.",
#'   "birth weight in grams."
#' )
#' d3 <- triplotly(birthwt[, -10], factors = 1,
#'                 group_by = "lower than 2.5 kg",
#'                 colorPalette = "Set2",
#'                 components = c(1, 2, 3), arr.scale = 0.02,
#'                 size = 4)
#' \dontrun{
#'   print(d3)
#' }
triplotly <- function(data, factors, group_by, components = c(1,2),
                      alpha = 0, title = "", arr.scale = 1, scale.pc = F,
                      colorPalette = "RdYlBu", opacity = 1, size = 1, showLabels) {
  
  nc <- length(components)
  assertthat::assert_that(nc == 2 | nc == 3,
                          msg = paste("components =", paste0(components, collapse = " "),
                                      "Please, select two or three components"))
  assertthat::assert_that(nc == length(unique(components)))
  
  if (all(class(data) != c("trp", "R6"))) {
    assertthat::assert_that(is.data.frame(data))
    # create svdtbl for plotting
    data <- trp$new(data, factors) #FIXME: Separate data and trp_obj
    data$doSVD()
    data$calcBi_df(
      components = components,
      group_by = group_by,
      alpha = alpha
    )
  }
  
  bi_df <- data$bi_df
  svd_obj <- data$svd_obj
  
  # save names 
  name_pc <- paste0("PC ", components)
  
  # change colnames to standardnames for plotly
  colnames(bi_df) <- c("group", paste0("G", 1:nc), 
                       paste0("H", 1:nc), "variable")
  
  # prepare axis labels for plot
  ppcvar <- data$ppcvar[components]
  if (scale.pc) {
    bi_df$PC_1 <- scale(bi_df$G1)
    bi_df$PC_2 <- scale(bi_df$G2)
    xlab <- paste("standardized ", name_pc[1], ppcvar[1], "% of Variance")
    ylab <- paste("standardized ", name_pc[2], ppcvar[2], "% of Variance")
    zlab <- paste("standardized ", name_pc[3], ppcvar[3], "% of Variance")
  } else {
    xlab <- paste(name_pc[1], "(", ppcvar[1], "% VAR)")
    ylab <- paste(name_pc[2], "(", ppcvar[2], "% VAR)")
    zlab <- paste(name_pc[3], "(", ppcvar[3], "% VAR)")
  }
  
  if (nc == 2) {
    # 2d plot
    p <- plotly::plot_ly(na.omit(bi_df)) %>%
      # arrow ends
      plotly::add_markers(x = ~H1 * arr.scale,
                          y = ~H2 * arr.scale,
                          type = 'scatter',
                          text = ~variable,
                          hoverinfo = "text",
                          showlegend = FALSE,
                          marker = list(symbol = "x",
                                        color = 'white', 
                                        alpha = '0.1')) %>%
      # plot the scaled loadings H (arrows)
      plotly::add_annotations(ax = 0, ay = 0,
                              x = ~H1 * arr.scale, 
                              y = ~H2 * arr.scale,
                              xref = "x", yref = "y",
                              axref = "x", ayref = "y",
                              showarrow = TRUE,
                              arrowhead = 2,
                              arrowsize = .1,
                              arrowcolor = "black",
                              text = "",
                              hoverinfo = "text",
                              alpha = 1,
                              color = I('green')) %>%
      # plot scaled PCs G
      plotly::add_markers(data = bi_df,
                          x = ~G1, y = ~G2,
                          color = ~group,  alpha = 1, type = 'scatter',
                          colors = colorPalette,
                          opacity = opacity,
                          hoverinfo = "none",
                          text = ~group,
                          marker = list(size = size)) %>%
      
      plotly::layout(xaxis = list(title = xlab),
                     yaxis = list(title = ylab),
                     title = title, 
                     legend=list(title=list(text=group_by)))
    
  } else if (nc == 3) {
    # 3d plot
    p <- plotly::plot_ly(na.omit(bi_df)) %>%
      
      # plot scaled PCs G
      plotly::add_markers(data = bi_df, 
                          x = ~G1,
                          y = ~G2,
                          z = ~G3,
                          color = ~group,  
                          opacity = opacity,
                          type = 'scatter',
                          colors = colorPalette,
                          hoverinfo = "none",
                          text = ~group,
                          marker = list(size = size)) %>%
      plotly::layout(scene = list(xaxis = list(title = xlab),
                                  yaxis = list(title = ylab),
                                  zaxis = list(title = zlab)),
                     title = title)
    for (i in 1:length(na.omit(bi_df$H1))) {
      p <- p %>% plotly::add_trace(type = "scatter3d", mode = "lines",
                                   line = list(color = "black", width = 4),
                                   showlegend = FALSE,
                                   hoverinfo = "text",
                                   text = bi_df$variable[i],
                                   x = c(0, bi_df$H1[i] * arr.scale),
                                   y = c(0, bi_df$H2[i] * arr.scale),
                                   z = c(0, bi_df$H3[i] * arr.scale)) %>% 
       plotly::add_trace(
         type = "cone",
         x = bi_df$H1[i] * arr.scale * 1.01,
         y = bi_df$H2[i] * arr.scale * 1.01,
         z = bi_df$H3[i] * arr.scale * 1.01,
         u = bi_df$H1[i] * arr.scale * 0.2,
         v = bi_df$H2[i] * arr.scale * 0.2,
         w = bi_df$H3[i] * arr.scale * 0.2,
         colorscale = list(list(0, '#000000'), list(1, '#000000')),
         anchor = "tip", #make cone tip be at endpoint
         hoverinfo = "none",
         showscale = FALSE
      )
    }
  }
  invisible(p)
}

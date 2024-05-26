

#' R6 constructor for the svd table
#'
#'
#' @export
#'
svd_tbl <- R6::R6Class(
  "svd_tbl",
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
    
    initialize = function(data, factors) {
      
      # assertthat::assert_that(all(factors %in% colnames(data)))
      self$factors <- factors
      # debug(self$data_sanity)
      self$data_sanity(data)
      # self$doSVD(data)
      # self$calcBi_df(components, group_by, alpha)
    },
    
    data_sanity = function(data_raw) {
      
      # sanity checks on the data set
      msg <- list()
      X <- data_raw %>% dplyr::select(!dplyr::all_of(self$factors))
      
      na_summary <- X %>%
        apply(2, function(x) sum(is.na(x)) * 100 / length(x)) 
      
      # Empty columns
      if(max(na_summary) == 100) {
        msg <- append(
          msg, 
          paste("Column(s)", paste0(names(na_summary)[na_summary == 100], collapse = "\n"),
                "are empty and will be removed")
        )
        
        data_raw <- data_raw %>%
          select(which(na_summary < 100))
      }
      
      if (!is.numeric(self$factors)) {
        assertthat::assert_that(
          all(self$factors %in% colnames(data_raw)), 
          msg = paste("The factors",
                      self$factors[which(!self$factors %in% colnames(data_raw))],
                      "do not match the data header..."))
      }
      # Rows with NAs
      data <- data_raw %>% tidyr::drop_na(all_of(colnames(X)))
      if(nrow(data) < nrow(data_raw)) {
        # Message for dropping rows
        msg <- append(
          msg, 
          list(
            paste0("We dropped ", nrow(data_raw) - nrow(data), " rows \n
            The following columns contain most missing values:", sep = "\n"),
            tibble(
              col = names(na_summary),
              pct_missing = na_summary
            ) %>% 
              arrange(desc(pct_missing)) %>%
              head()
          )
        )
      }
      # Constant columns
      X <- data %>% 
        dplyr::select(!dplyr::all_of(self$factors)) %>% 
        mutate(across(everything(), as.numeric))
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
      
      invisible(self)
    },
    
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
    # only update group_by
    change_col = function(group_by) {
      self$group_by <- group_by
      self$bi_df <- cbind(self$data[group_by], self$GH)
      invisible(self)
    },
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

#' PCA triplots
#'
#' @param data 
#' @param factors 
#' @param group_by 
#' @param components 
#' @param alpha 
#' @param title 
#' @param arr.scale 
#' @param scale.pc 
#' @param colorPalette 
#' @param opacity 
#'
#' @return a graphic
#' @export
#'
#' @examples
triplotly <- function(data, factors, group_by, components = c(1,2),
                      alpha = 0, title = "", arr.scale = 1, scale.pc = F,
                      colorPalette = "RdYlBu", opacity = 1, size = 1, showLabels) {
  
  nc <- length(components)
  assertthat::assert_that(nc == 2 | nc == 3,
                          msg = paste("components =", paste0(components, collapse = " "),
                                      "Please, select two or three components"))
  assertthat::assert_that(nc == length(unique(components)))
  
  if (all(class(data) != c("svd_tbl", "R6"))) {
    assertthat::assert_that(is.data.frame(data))
    # create svdtbl for plotting
    data <- svd_tbl$new(data, factors, group_by, components)
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

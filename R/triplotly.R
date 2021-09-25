
makeSVDtbl  <- function(data, color, components, alpha = 0) {
  
  if (color %in% colnames(data)) {
    cl <- color
    color <- data[color]
    data <- data[, colnames(data) != cl]
  } else if (is.character(color)) {
    stop(paste0("\"", color, "\" does not match any column of the data."))
  }
  
  
  # scale data & calculate single value decomposition
  Z <- scale(data, center = TRUE, scale = TRUE)
  svd_obj <- svd(Z)
  
  A <- svd_obj$v
  l <- svd_obj$d
  U <- svd_obj$u
  X <- U%*%diag(l)%*%t(A)
  
  # G = ULambda^alpha
  G <- as.data.frame(U[, components]%*%diag(l[components]^alpha))
  colnames(G) <- paste0("G", components)
  
  
  # H' = Lambda^(1-alpha)A'
  H <- as.data.frame(t(diag(l[components]^(1 - alpha)) %*% 
                         t(A[, components])))
  colnames(H) <- paste0("H", components)
  H <- data.frame(H, variable = colnames(data)) #column for variable names
  
  # Matrix of NAs
  na_mat <- matrix(rep(NA, (nrow(G) - nrow(H))* ncol(H)),
                   ncol = ncol(H))
  colnames(na_mat) <- colnames(H)
  
  H <- rbind(H, na_mat)
  
  # Include scaled 
  df <- cbind(color, G, H)
  colnames(df)[1] <- "col"
  
  return(list(df = df, 
              svd_obj = svd_obj))
}

triplotly <- function(data, color, components = c(1,2),
                     alpha = 0, title = "", arr.scale = 1, scale.pc = F,
                     colorPalette = "RdYlBu", opacity = 1) {
  
  nc <- length(components)
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(nc == 2 | nc == 3,
                          msg = paste("components =", components,
                                      "Please, select two or three components"))
  assertthat::assert_that(nc == length(unique(components)))
  
  # create dataframe for plotting
  svdtbl <- makeSVDtbl(data = data,
                      color = color,
                      components = components,
                      alpha = alpha)
  bi_df <- svdtbl$df
  svd_obj <- svdtbl$svd_obj
  
  # save names 
  name_pc <- paste0("Comp.", components)
  
  # change colnames to standardnames for plotly
  colnames(bi_df) <- c("col", paste0("G", 1:nc), 
                      paste0("H", 1:nc), "variable")
  
  # prepare axis labels for plot
  pcsd <- svd_obj$d/sqrt(nrow(svd_obj$u) - 1)
  pcvar <- pcsd^2
  pvar_pc <- round(pcvar[components] / sum(pcvar), digits = 4) * 100
  
  if (scale.pc) {
    bi_df$PC_1 <- scale(bi_df$G1)
    bi_df$PC_2 <- scale(bi_df$G2)
    xlab <- paste("standardized ", name_pc[1], pvar_pc[1], "% of Variance")
    ylab <- paste("standardized ", name_pc[2], pvar_pc[2], "% of Variance")
    zlab <- paste("standardized ", name_pc[3], pvar_pc[3], "% of Variance")
  } else {
    xlab <- paste(name_pc[1], "(", pvar_pc[1], "% of Variance)")
    ylab <- paste(name_pc[2], "(", pvar_pc[2], "% of Variance)")
    zlab <- paste(name_pc[3], "(", pvar_pc[3], "% of Variance)")
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
                          color = ~col,  alpha = 1, type = 'scatter',
                          colors = colorPalette,
                          opacity = opacity,
                          hoverinfo = "none",
                          text = ~col,
                          marker = list(size = 4)) %>%
      
      plotly::layout(xaxis = list(title = xlab),
                     yaxis = list(title = ylab),
                     title = title)
  } else if (nc == 3) {
    # 3d plot
    p <- plotly::plot_ly(na.omit(bi_df)) %>%
      # plot scaled PCs G
      plotly::add_markers(data = bi_df,
                          x = ~G1,
                          y = ~G2,
                          z = ~G3,
                          color = ~col,  opacity = opacity,
                          type = 'scatter',
                          colors = colorPalette,
                          hoverinfo = "none",
                          text = ~col,
                          marker = list(size = 2)) %>%
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
                          z = c(0, bi_df$H3[i] * arr.scale)) 
      }
  }

  print(p)
}

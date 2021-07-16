
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
  
  # pc.a <- choose[1]
  # pc.b <- choose[2]
  
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

biplotly <- function(data, color, components = c(1,2),
                     alpha = 0, title = "", arr.scale = 1, scale.pc = F,
                     colorPalette = "RdYlBu") {
  
  # create dataframe for plotting
  svdtbl <- makeSVDtbl(data = data,
                      color = color,
                      components = components,
                      alpha = alpha)
  bi_df <- svdtbl$df
  svd_obj <- svdtbl$svd_obj
  
  # save names 
  name_pc.a <- colnames(bi_df)[2]
  name_pc.b <- colnames(bi_df)[3]
  name_var.a <- colnames(bi_df)[4]
  name_var.b <- colnames(bi_df)[5]
  
  # change colnames to standardnames
  colnames(bi_df)[2:3] <- c("G1", "G2")
  colnames(bi_df)[4:5] <- c("H1", "H2")
  
  # prepare axis labels for plot
  pcsd <- svd_obj$d/sqrt(nrow(svd_obj$u) - 1)
  pcvar <- pcsd^2
  pvar_pc.1 <- round(pcvar[components[1]] / sum(pcvar), digits = 4) * 100
  pvar_pc.2 <- round(pcvar[components[2]] / sum(pcvar), digits =  4) * 100
  
  if (scale.pc) {
    bi_df$PC_1 <- scale(bi_df$G1)
    bi_df$PC_2 <- scale(bi_df$G2)
    xlab <- paste("standardized ", name_pc.a, pvar_pc.1, "% of Variance")
    ylab <- paste("standardized ", name_pc.b, pvar_pc.2, "% of Variance")
  } else {
    xlab <- paste(name_pc.a, "(", pvar_pc.1, "% of Variance)")
    ylab <- paste(name_pc.b, "(", pvar_pc.2, "% of Variance)")
  }
  
  # plot
  p <- plotly::plot_ly(bi_df) %>%
    # arrow ends
    plotly::add_markers(x = ~H1 * arr.scale,
                        y = ~H2 * arr.scale,
                        type = 'scatter',
                        text = ~variable,
                        name = "Variable",
                        marker = list(symbol = 'star-dot', color = 'black')) %>%
        # plot the scaled loadings H (arrows) 
    plotly::add_segments(x = 0, y = 0,
                         xend = ~H1 * arr.scale, 
                         yend = ~H2 * arr.scale,
                         name = "Loadings",
                         alpha = 1,
                         color = I('grey')) %>%
    # plot scaled PCs G
    plotly::add_markers(x = ~G1, y = ~G2,
                color = ~col,  alpha = 0.9, type = 'scatter',
                colors = colorPalette,
                text = ~col,
                marker = list(size = 4)) %>%
 
    plotly::layout(xaxis = list(title = xlab),
           yaxis = list(title = ylab),
           title = title)

  return(p)
}

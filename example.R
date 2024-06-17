
### Threads where to promote package: 
# https://stackoverflow.com/questions/10858904/extract-3d-coordinates-from-r-pca
# https://stackoverflow.com/questions/57076230/any-efficient-way-to-label-the-data-points-along-top-3-pca-in-3d-plot-in-r?rq=1
# https://stackoverflow.com/questions/10252639/pca-factominer-plot-data?rq=1

# Example data ------------------------------------------------------------
# library(dplyr)
# 
# data("iris")
# head(iris)
# triplotly(iris, color = "Species", components = c(1, 2), arr.scale = 0.02)
# 
# d <- makeSVDtbl(iris, color = "Species", components = c(1, 2, 3), alpha = 1)
# 
# # 3d
# triplotly(data = iris, color = "Species", components = c(1, 2, 3), alpha = 1)
# 
# data("mtcars")
# head(mtcars)
# 
# # mtcars$company <- rownames(mtcars) %>% 
# #   stringr::str_split(" ") %>% 
# #   sapply(function(x) {x[1]})
# 
# mtcars$cyl <- as.factor(mtcars$cyl)
# 
# triplotly(mtcars, color = "cyl",
#          components = c(1, 2), alpha = 1, 
#          arr.scale = 1)
# 
# triplotly(mtcars, color = "cyl",
#          components = c(1, 2, 3), alpha = 1, 
#          arr.scale = 1)
# 
# data("big5")
# big5$gender <- as.factor(big5$gender)
# 
# a <- triplotly(big5, factors = 1:5, group_by = "gender", color = "gender",
#          components = c(1, 2), alpha = 0.5, arr.scale = 0.001)
# a
# triplotly(big5[, c(4, 8:ncol(big5))], color = "gender",
#          components = c(1, 2, 3), alpha = 1,
#          arr.scale = 30, opacity = 0.1, 
# )


# bgg <- read.csv("C:/Users/phili/R_projects/BGG/Data_BGG.csv",
#                 header = TRUE, sep=';') %>%
#   as_tibble() %>%
#   filter(year == 2023) %>%
#   select(where(function(x) sum(is.na(x))/nrow(.) <= 0.4))
# 
# 
# write.csv(bgg,
#           file = "C:/Users/phili/R_projects/BGG/Data_BGG_2023.csv", 
#           row.names = F
#           )
# debug(tryUpload)
# 
# rv <- tryUpload(list(datapath = "C:/Users/phili/R_projects/BGG/Data_BGG_2023.csv"))
# rv <- tryUpload(list(datapath = "C:/Users/phili/R_projects/BGG/Data_BGG.csv"))
# 
# rv <- tryUpload(NULL)

# rv$data <- input_data$data
# # rv$group_opts <- input_data$group_opts
# # rv$start_group <- input_data$start_group
# # rv$msg <- input_data$msg
# rv$svdtbl <- triplotly::svd_tbl$new(
#   rv$data,
#   factors = rv$factors
# )
# 
# rv$svdtbl$doSVD()
# rv$svdtbl$calcBi_df(c(1, 2), group_by = rv$start_group, alpha =1)
# 
# data.frame(comp = 1:length(rv$svdtbl$pcvar),
#            variance = rv$svdtbl$pcvar,
#            p_var = rv$svdtbl$ppcvar,
#            cp_var = cumsum(rv$svdtbl$ppcvar))

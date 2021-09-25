
### Threads where to promote package: 
# https://stackoverflow.com/questions/10858904/extract-3d-coordinates-from-r-pca
# https://stackoverflow.com/questions/57076230/any-efficient-way-to-label-the-data-points-along-top-3-pca-in-3d-plot-in-r?rq=1
# https://stackoverflow.com/questions/10252639/pca-factominer-plot-data?rq=1

# Example data ------------------------------------------------------------
library(dplyr)

data("iris")
head(iris)
triplotly(iris, color = "Species", components = c(1, 2), arr.scale = 0.02)

d <- makeSVDtbl(iris, color = "Species", components = c(1, 2, 3), alpha = 1)

# 3d
triplotly(data = iris, color = "Species", components = c(1, 2, 3), alpha = 1)

data("mtcars")
head(mtcars)

# mtcars$company <- rownames(mtcars) %>% 
#   stringr::str_split(" ") %>% 
#   sapply(function(x) {x[1]})

mtcars$cyl <- as.factor(mtcars$cyl)

triplotly(mtcars, color = "cyl",
         components = c(1, 2), alpha = 1, 
         arr.scale = 1)

triplotly(mtcars, color = "cyl",
         components = c(1, 2, 3), alpha = 1, 
         arr.scale = 1)

# big 5 personality data
big5 <- read.csv("BIG5/data.csv", sep = "\t")
codes <- scan("BIG5/codebook.txt",
              what = "character",
              sep = "\n",
              skip = 4, nmax = 50) %>%
  lapply(function(x){unlist(stringr::str_split(x, "\\t"))[2]})

colnames(big5)[8:ncol(big5)] <- codes
big5$gender <- as.factor(big5$gender)

triplotly(big5[, c(4, 8:ncol(big5))], color = "gender",
         components = c(1, 2), alpha = 1, arr.scale = 10)

triplotly(big5[, c(4, 8:ncol(big5))], color = "gender",
         components = c(1, 2, 3), alpha = 1,
         arr.scale = 30, opacity = 0.1, 
)

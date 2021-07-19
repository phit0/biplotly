# Example data ------------------------------------------------------------
library(dplyr)

data("iris")
head(iris)

biplotly(iris, color = "Species", components = c(1, 2), arr.scale = 0.02)


data("mtcars")
head(mtcars)

# mtcars$company <- rownames(mtcars) %>% 
#   stringr::str_split(" ") %>% 
#   sapply(function(x) {x[1]})

mtcars$cyl <- as.factor(mtcars$cyl)

biplotly(mtcars, color = "cyl",
         components = c(2, 3), alpha = 1, 
         arr.scale = 1)



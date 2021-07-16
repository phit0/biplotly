# Example data ------------------------------------------------------------

data("iris")
head(iris)

biplotly(iris, color = "Species", components = c(1, 2), arr.scale = 0.02)


data("mtcars")
head(mtcars)

mtcars$company <- rownames(mtcars) %>% 
  stringr::str_split(" ") %>% 
  lapply(function(x) {x[1]})

debug(biplotly)
biplotly(mtcars, color = "company", components = c(1, 2), arr.scale = 0.02)

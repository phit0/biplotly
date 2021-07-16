# Example data ------------------------------------------------------------

data("iris")
head(iris)

biplotly(iris, color = "Species", components = c(1, 2), arr.scale = 0.02)

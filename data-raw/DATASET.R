## code to prepare `DATASET` dataset goes here
# big 5 personality data
big5 <- read.csv("data-raw/data.csv", sep = "\t")
codes <- scan("data-raw/codebook.txt",
              what = "character",
              sep = "\n",
              skip = 4, nmax = 50) %>%
  lapply(function(x){unlist(stringr::str_split(x, "\\t"))[2]})

colnames(big5)[8:ncol(big5)] <- codes

usethis::use_data(big5, overwrite = TRUE, compress = T)

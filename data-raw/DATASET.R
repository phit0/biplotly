## code to prepare `DATASET` dataset goes here
# big 5 personality data
library(dplyr)
big5 <- read.csv("data-raw/data.csv", sep = "\t")
codes <- scan("data-raw/codebook.txt",
              what = "character",
              sep = "\n",
              skip = 4, nmax = 50) %>%
  lapply(function(x){unlist(stringr::str_split(x, "\\t"))[2]})

colnames(big5)[8:ncol(big5)] <- codes
big5 <- dplyr::select(big5, !c("race","age"))
big5$gender[which(big5$gender == 0)] <- "missed"
big5$gender[which(big5$gender == 1)] <- "male"
big5$gender[which(big5$gender == 2)] <- "female"
big5$gender[which(big5$gender == 3)] <- "other"

big5$engnat[which(big5$engnat == 0)] <- "missed"
big5$engnat[which(big5$engnat == 1)] <- "yes"
big5$engnat[which(big5$engnat == 2)] <- "no"

big5$hand[which(big5$hand == 0)] <- "missed"
big5$hand[which(big5$hand == 1)] <- "Right"
big5$hand[which(big5$hand == 2)] <- "Left"
big5$hand[which(big5$hand == 3)] <- "Both"

big5$source <- as.factor(big5$source)
big5 <-  big5 %>% sample_n(size = 5000)

usethis::use_data(
  big5,
  overwrite = TRUE, compress = T)

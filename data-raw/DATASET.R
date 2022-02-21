## code to prepare `DATASET` dataset goes here

exData <- read.table("inst/exData.txt")
usethis::use_data(exData, overwrite = TRUE)

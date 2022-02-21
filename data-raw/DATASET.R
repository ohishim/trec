## code to prepare `DATASET` dataset goes here

#---   exported
exData <- read.table("inst/exData.txt")
usethis::use_data(exData, overwrite = TRUE)

#---  internal
EstForFitIcons <- read.csv("inst/coefficients.csv")
ImagesOfIcons <- lapply(1:10, function(j){readPNG(paste0("inst", "/", "icon", j, ".png"))})

usethis::use_data(EstForFitIcons, ImagesOfIcons, internal=T)

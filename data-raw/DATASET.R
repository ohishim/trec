## code to prepare `DATASET` dataset goes here

#---   exported
exData <- read.table("inst/exData.txt")
exData <- mutate(exData, year = 2001:2020) %>%
  dplyr::select(year, everything())
usethis::use_data(exData, overwrite = TRUE)

#---  internal
EstForFitIcons <- read.csv("inst/MLDcoef.csv") %>%
  mutate(
    group = factor(group, c("Downward", "Upward", "Flat"))
  ) %>% split(.$group) %>%
  lapply(function(x){select(x, X.Intercept.:c3) %>% data.matrix})
ImagesOfIcons <- lapply(1:10, function(j){readPNG(paste0("inst", "/", "icon", j, ".png"))})

usethis::use_data(EstForFitIcons, ImagesOfIcons, internal=T, overwrite = TRUE)

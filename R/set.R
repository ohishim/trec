
#estimates for fitting icons
BETA <- read.csv("data/coefficients.csv")

#images of icons
img <- lapply(1:10, function(j){readPNG(paste0("fig", "/", "icon", j, ".png"))})

#example dataset
exData <- read.csv("data/exData.csv")


#example dataset
exData <- read.table("data/exData.txt")

#images of icons
img <- lapply(1:10, function(j){readPNG(paste0("fig", "/", "icon", j, ".png"))})

#estimates for fitting icons
BETA <- read.csv("data/coefficients.csv")

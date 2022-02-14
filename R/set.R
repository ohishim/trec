
#example dataset
exData <- read.table("data/exData.txt")

#images of icons (used in 'TREC3')
ImagesOfIcons <- lapply(1:10, function(j){readPNG(paste0("fig", "/", "icon", j, ".png"))})

#estimates for fitting icons (used in 'icon.fit')
EstForFitIcons <- read.csv("data/coefficients.csv")

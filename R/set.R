BETA <- read.csv("data/coefficients.csv")
img <- lapply(1:10, function(j){readPNG(paste0("fig", "/", "icon", j, ".png"))})

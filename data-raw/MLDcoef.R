#2022/03/29

#上昇・平坦・下降のグループごとに多項ロジット判別器を作成する

rm(list=ls(all.names = T));  #変数のクリア

mainp <- "C:\\Users\\ohishim\\OneDrive - Hiroshima University\\statistics\\R\\package\\trec"
sourcep <- paste0(mainp, "\\", "R")

library(tidyverse)
library(magrittr)
library(dendextend)
library(rlist)
library(gt)
library(nnet)

source(paste0(sourcep, "\\", "ctrend.R"))
source(paste0(sourcep, "\\", "miss_inpol.R"))

################################################################################
###   path
################################################################################

datap <- "C:\\Users\\ohishim\\OneDrive - Hiroshima University\\statistics\\research\\TREC\\Data"

#すべてのデータのパス
alldatap <- c(
  #CPUE2019
  sapply(1:11, function(j){
    paste0("CPUE mean area ", j, ".txt") %>% paste0("CPUE2019\\", .)
  }),

  #CPUE2020
  sapply(1:11, function(j){
    paste0("CPUE_mean_area_", j, ".txt") %>% paste0("CPUE2020\\", .)
  }),

  #Oceanography
  sapply(1:14, function(j){
    paste0("mn_area", j, ".txt") %>% paste0("Oceanography\\", .)
  }),

  #WGIBAR1
  "Data update to 2022 WGIBAR_trec_m_2005-2021.txt",

  #WGIBAR2
  "Data_update_to_2022_WGIBAR_1980_2021_r.txt",

  #WGINOR
  "Data_WGINOR_2020_V5_Matlab_96_20.txt",

  #FO1
  "Edited_Data_FO_0220.txt",

  #FO2
  "Edited_Data_FO_0320r.txt",

  #FO3
  "Edited_Data_FO_9621.csv"
)

################################################################################
###   トレンドの推定と分類
################################################################################

# fig.trend0 <- tabD0 <- list()
Out0 <- list()

for(path.i in 1:length(alldatap))
{
  ##############################################################################
  ###   データの読み込み
  ##############################################################################

  if(path.i %in% 1:22)
  {#---   CPUE   ---------------------------------------------------------------

    Data0 <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=1)
    Data <- select(Data0, -V1, -V2)
    names(Data) <- paste0("V", 1:ncol(Data))

  } else if(path.i %in% 23:36)
  {#---   Oceanography   -------------------------------------------------------

    Data0 <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=1)
    Data <- select(Data0, -V1, -V2)
    names(Data) <- paste0("V", 1:ncol(Data))

  } else if(path.i == 37)
  {#---   WGIBAR1   ------------------------------------------------------------

    Data <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=1) %>%
      select(-V1) %>% set_names(paste0("V", 1:ncol(.)))
  } else if(path.i == 38)
  {#---   WGIBAR2   ------------------------------------------------------------

    Data <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=1)

  } else if(path.i == 39)
  {#---   WGINOR   -------------------------------------------------------------

    Data0 <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=2)
    Data <- select(Data0, -V1)
    names(Data) <- paste0("V", 1:ncol(Data))

  } else if(path.i %in% 40:41)
  {#---   FO1, 2   -------------------------------------------------------------

    Data0 <- read.table(paste(datap, alldatap[path.i], sep="\\"), skip=1)
    Data <- select(Data0, -V1)
    names(Data) <- paste0("V", 1:ncol(Data))
  } else if(path.i == 42)
  {#---   FO3   ----------------------------------------------------------------

    Data0 <- read.csv(paste(datap, alldatap[path.i], sep="\\"))
    Data <- select(Data0, -Year)
    names(Data) <- paste0("V", 1:ncol(Data))
  }

  ##############################################################################
  ###   トレンドの推定 (TREC1 より)
  ##############################################################################

  Y <- Data

  k <- ncol(Y)
  Labs <- paste0("V", 1:k)
  Y0 <- Y

  if(!all(Labs == names(Y0)))
  {
    Vnames <- data.frame(names(Y0) %>% t)
    names(Vnames) <- Labs

    names(Y) <- Labs
    names(Y0) <- paste0(Labs, " (", Vnames[1,], ")")

    cat("The variable names are represented as follows: \n")
    print(Vnames)
  } else
  {
    Vnames <- NULL
  }

  Y <- miss.inpol(Y) %>% scale

  idx.nan <- apply(Y, 2, function(y){all(is.nan(y))}) %>% which
  if(length(idx.nan) > 0){Y <- Y[,-idx.nan]}

  p <- ncol(Y)

  if(p < k)
  {
    idx.rm <- which(!Labs %in% colnames(Y))
    Vrm <- Vnames[1, idx.rm]
    Labs <- Labs[-idx.rm]

    if(!is.null(Vnames)){Vnames <- Vnames[,-idx.rm]}

    # cat("The following variable(s) is/are removed: \n")
    # print(Vrm)
  } else
  {
    Vrm <- NULL
  }

  res <- ctrend(Y)

  TR <- res$trend %>% t
  colnames(TR) <- colnames(Y)

  Out0[[path.i]] <- data.frame(
    data = path.i,
    V = Labs,
    res$dim %>% set_colnames(c("d1", "d2")),
    res$coef %>% set_colnames(paste0("c", 0:3))
  )

} #end for data.i

Out <- do.call(rbind, Out0) %>%
  mutate(
    idx = 1:nrow(.)
  )

tlab <- read.csv("inst/icon_assignment_210322.csv")

Data <- left_join(Out, tlab, by="idx") %>%
  select(idx, data, V, icon, group, everything()) %>%
  mutate(
    group = case_when(
      group == "down" ~ "Downward",
      group == "up" ~ "Upward",
      group == "flat" ~ "Flat",
      group == "" & icon %in% c(1, 6, 9) ~ "Flat",
      group == "" & icon %in% c(2, 7, 8) ~ "Downward",
      group == "" & icon %in% c(3, 4, 5) ~ "Upward"
    ) %>% factor(levels=c("Downward", "Upward", "Flat"))
  )

################################################################################
###   多項ロジット判別器作成
################################################################################

BETA <- split(Data, Data$group) %>% lapply(function(D){
  mlD <- select(D, icon, d1:c3)
  Beta <- multinom(icon ~ ., data=mlD) %>% coefficients
  return(
    data.frame(
      group = D$group %>% unique,
      base = D$icon %>% min,
      icon = rownames(Beta),
      Beta
    )
  )
}) %>% do.call(rbind, .) %>% set_rownames(NULL)

write.csv(BETA, "inst/MLDcoef.csv", row.names=F)

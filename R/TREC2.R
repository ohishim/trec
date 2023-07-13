#' @title The second step of TREC (v1.1.0)
#' @description \code{TREC2} This function performs rough classification of trends.
#'
#' @importFrom stats as.dendrogram cutree dist hclust
#' @importFrom graphics par
#' @importFrom magrittr %>% %$% set_names
#' @importFrom dendextend set
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 geom_line ggplot theme ylim
#' @importFrom plotly subplot
#' @importFrom dplyr arrange case_when everything mutate right_join select
#' @importFrom rlist list.findi
#' @param argTREC the output `argTREC` of `TREC1`
#' @param clustering if `TRUE`, clustering by a dendorogram (default);
#'   if `FALSE`, discrimination by the criterion
#' @param pvar two variable names for representative trends (option)
#' @param groups the number of groups for classification
#' @return a list of classification results which has the following elements:
#' \item{dend}{a fanction with no parameter to draw a dendrogram}
#'
#' \item{fig.trends}{a figure of estimated trends for each classified group}
#'
#' \item{trn}{a list of variable names for each classified group}
#' @export
#' @examples
#' #TREC2(argTREC)

TREC2 <- function(argTREC, clustering=TRUE, pvar=NULL, groups=2){

  TR <- argTREC$TR
  Labs <- colnames(TR)
  p <- ncol(TR)

  if(is.null(pvar))
  {
    a <- abs(TR) %>% max
    t1 <- seq(-a, a, length=nrow(TR))
    t2 <- rev(t1)
  } else
  {
    t1 <- TR[, which(Labs == pvar[1])]
    t2 <- TR[, which(Labs == pvar[2])]
  }

  dd <- sapply(1:p, function(j){
    sum((t1 - TR[,j])^2) - sum((t2 - TR[,j])^2)
  }) %>% set_names(Labs)

  Cols <- c( "#FC8D62", "#66C2A5", "#8DA0CB")[1:groups]
  Pchs <- c(19, 17, 15)[1:groups]

  if(clustering)
  {
    HClust <- dd %>% dist %>% hclust(method = "centroid")
    LabD <- as.dendrogram(HClust) %>% labels

    DUtr <- c(which.max(dd), which.min(dd)) %>% Labs[.]
    trn0 <- lapply(1:groups, function(j){which(cutree(HClust, k=groups) == j) %>% Labs[.]})

    trn <- lapply(1:groups, function(j){
      if(j < 3)
      {
        idx <- list.findi(trn0, DUtr[j] %in% .)
      } else
      {
        idx <- list.findi(trn0, !any(DUtr %in% .))
      }

      return(trn0[[idx]])
    }) %>% set_names(c("Downward", "Upward", "Flat")[1:groups])

    pchn <- lapply(1:groups, function(x){cbind(a=which(LabD %in% trn0[[x]]), b=x)}) %>%
      do.call(rbind, .) %>% as.data.frame %>% arrange(a) %>%
      mutate(b = factor(b, levels=unique(b))) %$% split(a, b) %>%
      sapply(length)

    Dend <- HClust %>% as.dendrogram %>%
      set("branches_k_color", k=groups, Cols) %>%
      set(
        "leaves_pch", mapply(rep, Pchs, pchn) %>% unlist
      ) %>%
      set("leaves_cex", 2) %>%
      set("branches_lwd", 3)

    out0 <- function(){plot(Dend)}
    Gidx <- 1:groups
  }

  if(!clustering)
  {
    if(groups == 2)
    {
      trni <- list(
        which(dd > 0),
        which(dd < 0)
      )

      trn <- lapply(trni, function(x){Labs[x]}) %>% set_names(
        c("Downward", "Upward")
      )
    }

    if(groups == 3)
    {
      t3 <- seq(-0.1, 0.1, length=nrow(TR))
      q <- abs(sum((t1 - t3)^2) - sum((t2 - t3)^2))

      trni <- list(
        which(dd > q),
        which(dd < -q),
        which(abs(dd) <= q)
      )

      trn <- lapply(trni, function(x){Labs[x]}) %>% set_names(
        c("Downward", "Upward", "Flat")
      )
    }

    Dend <- lapply(trni, function(x){
      if(length(x) >= 2)
      {
        dd[x] %>% dist %>% hclust(method = "centroid") %>% as.dendrogram %>% set()
      } else
      {
        NULL
      }
    })

    dendplot <- function(){
      idx <- sapply(trni, length)

      par(mfcol=c(1,sum(idx>1)))
      for(j in which(idx > 1))
      {
        plot(Dend[[j]], main=names(trn)[j])
      }
      par(mfcol=c(1,1))
    }

    out0 <- dendplot
    Gidx <- which(sapply(trn, length) > 0)

    na.idx <- which(sapply(trni, length) == 0)
    if(length(na.idx) > 0)
    {
      message("The following group(s) is/are not applicable: ")
      print(names(trn)[na.idx])

      for(i in na.idx)
      {
        trn[[i]] <- "not applicable"
      }
    }
  }

  ggD3 <- argTREC$ggD3

  ran <- range(ggD3$t)

  TRN <- lapply(Gidx, function(j){
    cbind(trn = j, V = trn[[j]])
  }) %>% do.call(rbind, .) %>% data.frame %>%
    mutate(V = factor(V, levels=Labs))

  Labs1 <- paste(TRN$trn, TRN$V, sep="-")

  TRN1 <- mutate(TRN, V1 = Labs1) %>%
    right_join(ggD3, by="V") %>%
    select(V0=V, V=V1, everything()) %>%
    mutate(
      V = factor(V, levels=Labs1),
      group = case_when(
        trn == 1 ~ "1.Downward",
        trn == 2 ~ "2.Upward",
        trn == 3 ~ "3.Flat"
      )
    )

  fig.trends <- subplot(
    lapply(Gidx, function(j){
      fig <- subset(TRN1, trn==j) %>%
        ggplot() +
        geom_line(aes(x=x, y=t, col=V)) +
        theme(
          axis.title = element_blank()
        ) +
        ylim(ran) +
        facet_wrap(~group)

      if(j != 1)
      {
        fig <- fig + theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      }

      return(fig)
    })
  )

  print(fig.trends)

  Out <- list(
    dend = out0,
    fig.trends = fig.trends,
    trn = trn
  )

  return(Out)
}

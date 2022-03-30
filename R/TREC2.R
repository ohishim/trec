#' @title The second step of TREC
#' @description \code{TREC2} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr set_names
#' @importFrom dendextend set
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ylim
#' @importFrom plotly subplot
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlist list.findi
#' @param argTREC the output "argTREC" of TREC1
#' @param method clustering method
#' @param pvar two variable names for representative trends (option)
#' @param groups the number of groups for classification
#' @return a dendrogram
#' @export
#' @examples
#' #TREC2(argTREC)

TREC2 <- function(argTREC, method=c("dend", "D"), pvar=NULL, groups=2){

  method <- method[1]

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

  if(method == "dend")
  {
    HClust <- dd %>% dist %>% hclust(method = "centroid")
    Dend <- HClust %>% as.dendrogram %>% set("branches_k_color", k=groups)

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

    out0 <- function(){plot(Dend)}
    Gidx <- 1:groups
  }

  if(method == "D")
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

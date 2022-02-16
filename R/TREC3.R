#' @title The third step of TREC
#' @description \code{TREC3} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 annotation_raster
#' @importFrom ggplot2 ylim
#' @importFrom tidyr gather
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom png readPNG
#' @importFrom gridExtra grid.arrange
#' @param tvar a vector of variable names for target trends.
#' @param argTREC the output "argTREC" of TREC1.
#' @return some figures for trends and assigned icons
#' @export
#' @examples
#' #TREC3(tnum, argTREC)

TREC3 <- function(tvar, argTREC){

  ggD3 <- argTREC$ggD3

  ##############################################################################
  ###   Set target trend corresponding to icon
  ##############################################################################

  #=============================================================================
  ###   clustering based on the target trends
  #=============================================================================

  TR <- argTREC$TR
  Labs <- colnames(TR)
  p <- ncol(TR)

  tnum <- which(Labs %in% tvar)
  tvar <- Labs[tnum]

  TGTR <- TR[,tnum]

  L <- sapply(1:p, function(j){
    if(j %in% tnum)
    {
      out <- j
    } else
    {
      tj <- TR[,j]
      out <- (TGTR - tj) %>% apply(2, function(x){sum(x^2)}) %>% which.min %>% tnum[.]
    }
    return(out)
  }) %>% Labs[.]

  ggD4 <- data.frame(
    V = ggD3$V %>% unique,
    L = factor(L, levels=Labs)
  ) %>% left_join(ggD3, ., by="V")

  fig.tgtrend.G <- ggplot(ggD4) +
    geom_line(aes(x=x, y=t, col=V)) +
    facet_wrap(.~L) +
    theme(axis.title = element_blank())

  #=============================================================================
  ###   the target trend plots
  #=============================================================================

  fig.tgtrend <- ggD4 %>% subset(V %in% tvar) %>%
    ggplot() +
    geom_line(aes(x=x, y=t)) +
    facet_wrap(.~V) +
    theme(axis.title = element_blank())

  ##############################################################################
  ###   Assign icon to the target trends
  ##############################################################################

  Y <- argTREC$Y

  res <- data.frame(
    V = colnames(Y[,tnum]),
    icon = cbind(1, argTREC$dim, argTREC$coef)[tnum,] %>%  apply(1, icon.fit),
    argTREC$TR[,tnum] %>% t
  )

  n <- ncol(res) - 2
  p <- nrow(res)
  ggD <- gather(res, 3:(n+2), key="t", value="y") %>%
    mutate(
      x = seq(0, 1, length=n) %>% rep(each=p),
      V = factor(V, levels=unique(V))
    )

  yr <- ggD$y %>% range

  tvar.n <- length(tvar)

  figs <- lapply(1:tvar.n, function(j){
    ggDj <- subset(ggD, V==tvar[j])

    figj <- ggplot() +
      geom_line(data=ggDj, aes(x=x, y=y)) +
      facet_wrap(.~V) +
      annotation_raster(
        ImagesOfIcons[[res$icon[j]]],
        xmin = -Inf, xmax = 0.15, ymax = Inf,
        ymin = yr[2] - (3*(yr[2] - yr[1])/10)
      ) +
      theme(axis.title = element_blank()) +
      ylim(yr)
  })

  if(tvar.n <= 3)
  {
    fig.col <- tvar.n
  } else if(tvar.n == 4)
  {
    fig.col <- 2
  } else
  {
    fig.col <- 3
  }

  fig.icon <- do.call(
    grid.arrange,
    c(figs, list(ncol=fig.col))
  )

  ##############################################################################
  ###   Output
  ##############################################################################

  Out <- list(
    fig.tgtrend.G = fig.tgtrend.G,
    fig.tgtrend = fig.tgtrend,
    fig.icon = fig.icon,
    group = split(Labs, L)
  )

  cat("variables for each group: \n")
  print(Out$group)

  return(Out)
}

#' @title The first step of TREC
#' @description \code{TREC1} This function estimates cubic trends for multivariate time series observations.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @param Y an observation matrix. The row corresponds to time steps and the column corresponds to variables.
#' @return some figures for observed data and estimated trend, and arguments for next steps.
#' @export
#' @examples
#' #TREC1(Y)

TREC1 <- function(Y){

  Y0 <- Y

  ##############################################################################
  ###   Raw data plots
  ##############################################################################

  k <- ncol(Y0)

  ggD1 <- data.frame(
    x = seq(0, 1, length=nrow(Y0)),
    Y0
  ) %>% gather(2:(k+1), key="V", value="y") %>%
    mutate(V = factor(V, levels=colnames(Y0)))

  if(k <= 16)
  {
    fig.RawData <- ggplot(ggD1, aes(x=x, y=y)) +
      geom_line() +
      facet_wrap(. ~ V, scales="free") +
      theme(axis.title = element_blank())
  } else
  {
    div <- split(paste0("V", 1:k), rep(1:ceiling(k/16), each=16)[1:k])

    fig.RawData <- lapply(1:length(div), function(j){
      subset(ggD1, V %in% div[[j]]) %>% ggplot(aes(x=x, y=y)) +
        geom_line() +
        facet_wrap(. ~ V, scales="free") +
        theme(axis.title = element_blank())
    })
  }

  ##############################################################################
  ###   Missing interpolation and standardization
  ##############################################################################

  Y <- miss.inpol(Y0) %>% scale

  idx.nan <- apply(Y, 2, function(y){all(is.nan(y))}) %>% which
  if(length(idx.nan) > 0){Y <- Y[,-idx.nan]}

  ##############################################################################
  ###   Standardization plots
  ##############################################################################

  p <- ncol(Y)

  ggD2 <- data.frame(
    x = seq(0, 1, length=nrow(Y)),
    Y
  ) %>% gather(2:(p+1), key="V", value="y") %>%
    mutate(V = factor(V, levels=colnames(Y)))

  if(p <= 16)
  {
    fig.StdData <- ggplot(ggD2, aes(x=x, y=y)) +
      geom_line() +
      facet_wrap(. ~ V, scales="free") +
      theme(axis.title = element_blank())
  } else
  {
    div <- split(colnames(Y), rep(1:ceiling(p/16), each=16)[1:p])

    fig.StdData <- lapply(1:length(div), function(j){
      subset(ggD2, V %in% div[[j]]) %>% ggplot(aes(x=x, y=y)) +
        geom_line() +
        facet_wrap(. ~ V, scales="free") +
        theme(axis.title = element_blank())
    })
  }

  ##############################################################################
  ###   Detrending by polynomial trend model
  ##############################################################################

  res <- ctrend(Y)

  TR <- res$trend %>% t
  colnames(TR) <- colnames(Y)

  ggD3 <- as.data.frame(TR) %>% gather(1:p, key="V", value="t") %>% mutate(
    V = ggD2$V,
    y = ggD2$y,
    x = ggD2$x
  )

  if(p <= 16)
  {
    fig.ctrend <- ggplot(ggD3) +
      geom_line(aes(x=x, y=y)) +
      geom_line(aes(x=x, y=t), col="red")+
      facet_wrap(. ~ V, scale="free") +
      theme(axis.title = element_blank())
  } else
  {
    fig.ctrend <- lapply(1:length(div), function(j){
      subset(ggD3, V %in% div[[j]]) %>% ggplot() +
        geom_line(aes(x=x, y=y)) +
        geom_line(aes(x=x, y=t), col="red") +
        facet_wrap(. ~ V, scales="free") +
        theme(axis.title = element_blank())
    })
  }

  ##############################################################################
  ###   Trend plots
  ##############################################################################

  fig.trend <- ggplot(ggD3) +
    geom_line(aes(x=x, y=t, col=V), size=2) +
    theme(axis.title = element_blank())

  ##############################################################################
  ###   Output
  ##############################################################################

  argTREC <- list(
    TR = TR,
    ggD3 = ggD3,
    Y = Y,
    dim = res$dim,
    coef = res$coef
  )

  Out <- list(
    fig.RawData = fig.RawData,
    fig.StdData = fig.StdData,
    fig.ctrend = fig.ctrend,
    fig.trend = fig.trend,
    argTREC = argTREC
  )

  return(Out)
}

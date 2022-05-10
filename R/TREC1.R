#' @title The first step of TREC
#' @description \code{TREC1} This function estimates cubic trends for multivariate time series observations.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_ribbon
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom plotly ggplotly
#' @param Y an observation matrix or data.frame; the row corresponds to time steps and the column corresponds to variables.
#' @param time.points a column name of `Y` expressing time points, if need; default is `NULL`.
#' @return a list of results for trend estimation which has the following elements:
#' \item{fig.RawData}{a figure of raw data plot;
#'     if the number of variables is larger than 16, this is a list of figures}
#'
#' \item{fig.StdData}{a figure of standardized data plot;
#'     if the number of variables is larger than 16, this is a list of figures}
#'
#' \item{fig.ctrend}{a figure of estimated trends with prediction band for each variable;
#'     if the number of variables is larger than 16, this is a list of figures}
#'
#' \item{fig.trend}{a figure of all estimated trends}
#'
#' \item{argTREC}{a list required in `TREC2`}
#'
#' \item{remove}{removed variable names}
#'
#' \item{Vnames}{a relationship between the original variable names and
#'     the represented variable names}
#' \item{empty}{empty variable names}
#' @export
#' @examples
#' #TREC1(Y)

TREC1 <- function(Y, time.points=NULL){

  if(is.null(time.points))
  {
    time.points <- 1:nrow(Y)
  } else
  {
    time.idx <- which(colnames(Y) == time.points)
    time.points <- Y[,time.idx]
    Y <- Y[,-time.idx]
  }

  empty <- apply(Y, 2, function(x){all(is.na(x) | is.nan(x))}) %>% which

  if(length(empty) > 0)
  {
    empvar <- colnames(Y)[empty]
    Y <- Y[,-empty]

    message("The following variable(s) is/are removed because of empty.")
    print(empvar)
  } else
  {
    empvar <- NULL
  }

  k <- ncol(Y)
  Labs <- paste0("V", 1:k)
  if(is.null(colnames(Y))){colnames(Y) <- Labs}
  Y0 <- Y

  if(!all(Labs == names(Y0)))
  {
    Vnames <- data.frame(names(Y0) %>% t)
    names(Vnames) <- Labs

    names(Y) <- Labs
    names(Y0) <- paste0(Labs, " (", Vnames[1,], ")")

    message("The variable names are represented as follows:")
    print(Vnames)
  } else
  {
    Vnames <- NULL
  }

  ##############################################################################
  ###   Raw data plots
  ##############################################################################

  Labs0 <- names(Y0)
  ggD1 <- data.frame(
    x = time.points,
    Y0
  )
  names(ggD1) <- c("x", names(Y0))
  ggD1 <- ggD1 %>% gather(2:(k+1), key="V", value="y") %>%
    mutate(V = factor(V, levels=names(Y0)))

  if(k <= 16)
  {
    fig.RawData <- ggplot(ggD1, aes(x=x, y=y)) +
      geom_line() +
      facet_wrap(. ~ V, scales="free_y") +
      theme(
        axis.title = element_blank(),
        legend.title = element_blank()
      )
  } else
  {
    div.n <- ceiling(k / 16)

    if(k %% div.n == 0)
    {
      div <- split(
        Labs0,
        rep(1:div.n, each=k / div.n)
      )
    } else
    {
      div <- split(
        Labs0,
        c(
          rep(1:div.n, k %/% div.n),
          1:(k %% div.n)
        ) %>% sort
      )
    }

    fig.RawData <- lapply(1:length(div), function(j){
      subset(ggD1, V %in% div[[j]]) %>% ggplot(aes(x=x, y=y)) +
        geom_line() +
        facet_wrap(. ~ V, scales="free_y") +
        theme(
          axis.title = element_blank(),
          legend.title = element_blank()
        )
    })
  }

  ##############################################################################
  ###   Missing interpolation and standardization
  ##############################################################################

  Y <- miss.inpol(Y) %>% scale

  p <- ncol(Y)

  if(p < k)
  {
    idx.rm <- which(!Labs %in% colnames(Y))
    Vrm <- Vnames[1, idx.rm]
    Labs <- Labs[-idx.rm]

    if(!is.null(Vnames)){Vnames <- Vnames[,-idx.rm]}

    message("The following variable(s) is/are removed:")
    print(Vrm)
  } else
  {
    Vrm <- NULL
  }

  ##############################################################################
  ###   Standardization plots
  ##############################################################################

  ggD2 <- data.frame(
    x = time.points,
    Y
  ) %>% gather(2:(p+1), key="V", value="y") %>%
    mutate(V = factor(V, levels=colnames(Y)))

  if(p <= 16)
  {
    fig.StdData <- ggplot(ggD2, aes(x=x, y=y)) +
      geom_line() +
      facet_wrap(. ~ V) +
      theme(
        axis.title = element_blank(),
        legend.title = element_blank()
      )
  } else
  {
    div.n <- ceiling(p / 16)

    if(p %% div.n == 0)
    {
      div <- split(
        Labs,
        rep(1:div.n, each=p / div.n)
      )
    } else
    {
      div <- split(
        Labs,
        c(
          rep(1:div.n, p %/% div.n),
          1:(p %% div.n)
        ) %>% sort
      )
    }

    fig.StdData <- lapply(1:length(div), function(j){
      subset(ggD2, V %in% div[[j]]) %>% ggplot(aes(x=x, y=y)) +
        geom_line() +
        facet_wrap(. ~ V) +
        theme(
          axis.title = element_blank(),
          legend.title = element_blank()
        )
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
    x = ggD2$x,
    w = as.vector(t(res$pbw))
  )

  if(p <= 16)
  {
    fig.ctrend <- ggplot(ggD3) +
      geom_ribbon(aes(x=x, ymin=t-w, ymax=t+w), alpha=0.2) +
      geom_line(aes(x=x, y=y)) +
      geom_line(aes(x=x, y=t), col="red")+
      facet_wrap(. ~ V) +
      theme(
        axis.title = element_blank(),
        legend.title = element_blank()
      )
  } else
  {
    fig.ctrend <- lapply(1:length(div), function(j){
      subset(ggD3, V %in% div[[j]]) %>% ggplot() +
        ggplot2::geom_ribbon(aes(x=x, ymin=t-w, ymax=t+w), alpha=0.2) +
        geom_line(aes(x=x, y=y)) +
        geom_line(aes(x=x, y=t), col="red") +
        facet_wrap(. ~ V) +
        theme(
          axis.title = element_blank(),
          legend.title = element_blank()
        )
    })
  }

  ##############################################################################
  ###   Trend plots
  ##############################################################################

  fig.trend <- ggplot(ggD3) +
    geom_line(aes(x=x, y=t, col=V)) +
    theme(
      axis.title = element_blank()
    )

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
    fig.trend = ggplotly(fig.trend),
    argTREC = argTREC,
    remove = Vrm,
    Vnames = Vnames,
    empty = empvar
  )

  print(Out$fig.trend)

  return(Out)
}

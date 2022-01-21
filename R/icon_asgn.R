#' @title Calculate fitted values
#' @description \code{icon.fit} This function calculates fitted values.
#'
#' @importFrom magrittr %>%
#' @param x a vector of explanatory variables.
#' @return a fitted value.
#' @examples
#' #icon.fit(x)

icon.fit <- function(x){

  if((x[1] == 1) & (abs(x[4]) <= 0.1))
  {
    out <- 1
  } else
  {
    q <- apply(BETA, 1, function(b){sum(b*x)})
    out <- (c(1, exp(q)) / (1 + sum(exp(q)))) %>% which.max
  }

  return(out)
}

#' @title Assign icons
#' @description \code{icon.asgn} This function assigns icons to cubic trends.
#'
#' @importFrom magrittr %>%
#' @param Z an observation matrix. The row corresponds to time steps and the column corresponds to variables.
#' @return a data.frame. This has the following items:
#'     V which is names of variables, icon which is assigned icon, and fitted values of trends.
#' @export
#' @examples
#' #icon.asgn(Z)

icon.asgn <- function(Z){

  ##############################################################################
  ###   missing interpolation and scaling
  ##############################################################################

  Y <- miss.inpol(Z) %>% scale

  idx.nan <- apply(Y, 2, function(y){all(is.nan(y))}) %>% which
  if(length(idx.nan) > 0){Y <- Y[,-idx.nan]}

  ##############################################################################
  ###   trend estimation
  ##############################################################################

  res <- ctrend(Y)

  out <- data.frame(
    V = colnames(Y),
    icon = cbind(1, res$dim, res$coef) %>%  apply(1, icon.fit),
    res$trend
  )

  return(out)
}

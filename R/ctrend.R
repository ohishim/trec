#' @title Estimate cubic trend
#' @description \code{ctrend} This function estimates cubic trends.
#'
#' @importFrom magrittr %>%
#' @param Y an observation matrix. The row corresponds to time steps and the column corresponds to variables.
#' @return trend: a matrix of fitted values.
#' @return dim: a matrix of dimensions of estimated trends.
#' @return coef: a matrix of estimates.
#' @examples
#' #ctrend(Y)

ctrend <- function(Y){

  n <- nrow(Y); p <- ncol(Y)

  x <- seq(0, 1, length=n)
  X <- cbind(1, x, x^2, x^3)
  X. <- t(X)

  Minv <- lapply(2:4, function(j){
    solve(X.[1:j,] %*% X[,1:j])
  })
  X.Y <- X. %*% Y

  res <- lapply(1:3, function(j){
    Betaj <- Minv[[j]] %*% X.Y[1:(1+j),]
    Y.hatj <- X[,1:(1+j)] %*% Betaj

    logl <- - (n-5) / 2 * log(apply(Y.hatj - Y, 2, var))
    AIC <- -2 * logl + 2*(1+j)

    return(list(
      coef = Betaj,
      AIC = AIC,
      trend = Y.hatj
    ))
  })

  Beta <- lapply(res, function(x){x$coef})
  AIC <- lapply(res, function(x){x$AIC}) %>% do.call(rbind, .)
  TREND <- lapply(res, function(x){x$trend})

  opt <- apply(AIC, 2, which.min)

  Dim <- lapply(1:p, function(j){
    d <- opt[j]

    dim <- numeric(2)

    if(d < 3)
    {
      dim[d] <- 1
    }

    return(dim)
  }) %>% do.call(rbind, .)

  Coef <- lapply(1:p, function(j){
    d <- opt[j]

    coef <- Beta[[d]][,j]

    if(d < 3)
    {
      coef <- c(coef, numeric(3-d))
    }

    return(coef)
  }) %>% do.call(rbind, .)

  trend <- lapply(1:p, function(j){
    TREND[[opt[j]]][,j]
  }) %>% do.call(rbind, .) %>% data.frame
  colnames(trend) <- paste0("t", 1:n)

  out <- list(
    trend = trend,
    dim = Dim,
    coef = Coef
  )

  return(out)
}

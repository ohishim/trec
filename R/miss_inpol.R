#' @importFrom stats approx
#' @importFrom magrittr %>%

miss.inpol <- function(X, remove.num){

  is.miss <- function(x){is.nan(x) | is.na(x)}

  n <- nrow(X)

  miss.n <- apply(X, 2, function(x){sum(is.miss(x))})

  if(all(miss.n == 0))
  {
    return(X)
  } else
  {
    idx <- which(0 < miss.n & miss.n <= remove.num) %>%
      sapply(function(j){
        if(!(X[c(1,n), j] %>% is.miss %>% any)){return(j)}
      }) %>% unlist

    if(length(idx) > 0)
    {
      X.inpol <- sapply(idx, function(j){
        x <- X[,j]
        idx.miss <- which(is.miss(x))
        x[idx.miss] <- approx(x, xout=idx.miss)$y

        return(x)
      })

      X[,idx] <- X.inpol
      idx.out <- c(
        idx, which(miss.n == 0)
      ) %>% sort
    } else
    {
      idx.out <- which(miss.n == 0)
    }

    return(X[,idx.out,drop=F])
  }
}


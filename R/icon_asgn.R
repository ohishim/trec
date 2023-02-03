#' @importFrom magrittr %>%

icon.fit <- function(x, group){

  out <- NULL

  if(group == "Downward")
  {
    icons <- c(2, 7, 8, 10)

    if((x[2] == 1) & (x[5] < 0))
    {
      out <- 7
    }
  }

  if(group == "Upward")
  {
    icons <- c(3, 4, 5, 10)

    if((x[2] == 1) & (x[5] > 0))
    {
      out <- 4
    }
  }

  if(group == "Flat")
  {
    icons <- c(1, 6, 9, 10)

    if((x[2] == 1) & (abs(x[5]) <= 0.1))
    {
      out <- 1
    }
  }

  if(is.null(out))
  {
    q <- apply(EstForFitIcons[[group]], 1, function(b){sum(b*x)})
    out <- (c(1, exp(q)) / (1 + sum(exp(q)))) %>% which.max %>% icons[.]
  }

  return(out)
}

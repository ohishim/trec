#' @title The second step of TREC
#' @description \code{TREC2} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom dendextend set
#' @importFrom gridExtra grid.arrange
#' @param pnum a vector of two indexes of variables for representative trends
#' @param argTREC the output "argTREC" of TREC1
#' @return a dendrogram
#' @export
#' @examples
#' #TREC2(pnum, argTREC)

TREC2 <- function(pnum, argTREC){
  TR <- argTREC$TR
  p <- ncol(TR)

  t1 <- TR[,pnum[1]]
  t2 <- TR[,pnum[2]]

  dd <- sapply(1:p, function(j){
    sum((t1 - TR[,j])^2) - sum((t2 - TR[,j])^2)
  })

  HClust <- dd %>% dist %>% hclust

  Dend <- HClust %>% as.dendrogram %>% set("branches_k_color", k=2)

  trn <<- lapply(1:2, function(j){which(cutree(HClust, k=2) == j)})

  plot(Dend)

  Out <- list(
    dend = Dend
  )

  ans <- ""
  while(!ans %in% c("yes", "no"))
  {
    ans <- readline("Do you want to more concrete classification? Please enter yes or no: ")
  }

  if(ans == "yes")
  {
    names(trn) <- c("trn1", "trn2")

    cat(
      "The variables are divided the following two groups:\n"
    )
    print(trn)

    ans1 <- ""
    while(!ans1 %in% c("yes", "no"))
    {
      ans1 <- readline("Do you need some modifications? Please enter yes or no: ")
    }

    if(ans1 == "no")
    {
      ggD3 <- argTREC$ggD3

      fig.trend1 <- ggD3 %>% subset(V %in% paste0("V", trn[[1]])) %>% ggplot() +
        geom_line(aes(x=x, y=t, col=V)) +
        theme(axis.title = element_blank())

      fig.trend2 <- ggD3 %>% subset(V %in% paste0("V", trn[[2]])) %>% ggplot() +
        geom_line(aes(x=x, y=t, col=V)) +
        theme(axis.title = element_blank())

      cat("Select tnum and proceed TREC3.\n")
      cat("You have 'trn' object for TREC3.")

      fig.trends <- grid.arrange(fig.trend1, fig.trend2, ncol=2)

      Out[[2]] <- fig.trends
      names(Out)[2] <- "fig.trends"
    } else
    {
      trn1 <<- trn[[1]]
      trn2 <<- trn[[2]]

      cat("You can use three objects 'trn', 'trn1', and 'trn2' to modify the groups.\n")
      cat("Redefine 'trn' and execute TREC2.1.")
    }

  } else
  {
    cat("trec procedure terminates.\n")
    cat("You have group numbers as 'trn' object.")
  }

  return(Out)
}

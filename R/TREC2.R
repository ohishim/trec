#' @title The second step of TREC
#' @description \code{TREC2} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom dendextend set
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ylim
#' @param pnum a vector of two indexes of variables for representative trends
#' @param argTREC the output "argTREC" of TREC1
#' @return a dendrogram
#' @export
#' @examples
#' #TREC2(pnum, argTREC)

TREC2 <- function(pnum, argTREC, groups=2){
  TR <- argTREC$TR
  # Vnames <- argTREC$Vnames
  p <- ncol(TR)

  t1 <- TR[,pnum[1]]
  t2 <- TR[,pnum[2]]

  dd <- sapply(1:p, function(j){
    sum((t1 - TR[,j])^2) - sum((t2 - TR[,j])^2)
  })
  names(dd) <- Vnames

  HClust <- dd %>% dist %>% hclust(method = "centroid")

  Dend <- HClust %>% as.dendrogram %>% set("branches_k_color", k=groups)

  trn <<- lapply(1:groups, function(j){which(cutree(HClust, k=groups) == j)})

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
    names(trn) <- paste0("trn", 1:groups)

    cat(
      "The variables are divided the following two groups:\n"
    )
    print(trn)

    ggD3 <- argTREC$ggD3

    ran <- range(ggD3$t)

    fig.trend1 <- ggD3 %>% subset(V %in% names(trn[[1]])) %>% ggplot() +
      geom_line(aes(x=x, y=t, col=V)) +
      theme(axis.title = element_blank()) +
      ylim(ran)

    fig.trend2 <- ggD3 %>% subset(V %in% names(trn[[2]])) %>% ggplot() +
      geom_line(aes(x=x, y=t, col=V)) +
      theme(axis.title = element_blank()) +
      ylim(ran)

    if(groups == 2)
    {
      fig.trends <- grid.arrange(fig.trend1, fig.trend2, ncol=2)
    } else if(groups == 3)
    {
      fig.trend3 <- ggD3 %>% subset(V %in% names(trn[[3]])) %>% ggplot() +
        geom_line(aes(x=x, y=t, col=V)) +
        theme(axis.title = element_blank()) +
        ylim(ran)

      fig.trends <- grid.arrange(fig.trend1, fig.trend2, fig.trend3, ncol=3)
    }

    ans1 <- ""
    while(!ans1 %in% c("yes", "no"))
    {
      ans1 <- readline("Do you need some modifications? Please enter yes or no: ")
    }

    if(ans1 == "no")
    {
      Out[[2]] <- fig.trends
      names(Out)[2] <- "fig.trends"

      cat("Select tnum and proceed TREC3.\n")
      cat("You have 'trn' object for TREC3.\n")
    } else
    {
      trn1 <<- trn[[1]]
      trn2 <<- trn[[2]]
      if(groups==3){trn3 <<- trn[[3]]}

      cat("You can use three objects 'trn', 'trn1', and 'trn2' to modify the groups.\n")
      cat("Redefine 'trn' and execute TREC2.1.\n")
    }

  } else
  {
    cat("trec procedure terminates.\n")
    cat("You have group numbers as 'trn' object.\n")
  }

  return(Out)
}

#' @title The second step of TREC
#' @description \code{TREC2} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom dendextend set
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

  dd %>% dist %>% hclust %>% as.dendrogram %>% set("branches_k_color", k=2) %>% plot
}

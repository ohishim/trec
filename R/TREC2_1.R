#' @title The additional step of the second step of TREC
#' @description \code{TREC2.1} This function draws trends for each group.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ylim
#' @importFrom gridExtra grid.arrange
#' @param trn a list that has thee elements of which each element is a vector of variable labels for increasing/flat/decreasing trends.
#' @param argTREC the output "argTREC" of TREC1
#' @param groups The number of groups corresponding to "trn".
#' @return a figure
#' @export
#' @examples
#' #TREC2.1(trn, argTREC)

TREC2.1 <- function(trn, argTREC, groups=2){

  ggD3 <- argTREC$ggD3

  ran <- range(ggD3$t)

  fig.trend1 <- ggD3 %>% subset(V %in% trn[[1]]) %>% ggplot() +
    geom_line(aes(x=x, y=t, col=V)) +
    theme(axis.title = element_blank()) +
    ylim(ran)

  fig.trend2 <- ggD3 %>% subset(V %in% trn[[2]]) %>% ggplot() +
    geom_line(aes(x=x, y=t, col=V)) +
    theme(axis.title = element_blank()) +
    ylim(ran)

  if(groups == 2)
  {
    grid.arrange(fig.trend1, fig.trend2, ncol=2)
  } else if(groups == 3)
  {
    fig.trend3 <- ggD3 %>% subset(V %in% trn[[3]]) %>% ggplot() +
      geom_line(aes(x=x, y=t, col=V)) +
      theme(axis.title = element_blank()) +
      ylim(ran)

    grid.arrange(fig.trend1, fig.trend2, fig.trend3, ncol=3)
  }

  cat("If you have no problem, select tvar and proceed TREC3.\n")
}

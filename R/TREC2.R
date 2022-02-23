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
#' @importFrom plotly subplot
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @param argTREC the output "argTREC" of TREC1
#' @param pvar two variable names for representative trends (option)
#' @param groups the number of groups for classification
#' @return a dendrogram
#' @export
#' @examples
#' #TREC2(argTREC)

TREC2 <- function(argTREC, pvar=NULL, groups=2){

  TR <- argTREC$TR
  Labs <- colnames(TR)
  p <- ncol(TR)

  if(is.null(pvar))
  {
    x <- seq(0, 1, length=nrow(TR))
    t1 <- x - 0.5
    t2 <- - x + 0.5
  } else
  {
    t1 <- TR[, which(Labs == pvar[1])]
    t2 <- TR[, which(Labs == pvar[2])]
  }

  dd <- sapply(1:p, function(j){
    sum((t1 - TR[,j])^2) - sum((t2 - TR[,j])^2)
  })
  names(dd) <- Labs

  HClust <- dd %>% dist %>% hclust(method = "centroid")

  Dend <- HClust %>% as.dendrogram %>% set("branches_k_color", k=groups)

  trn <<- lapply(1:groups, function(j){which(cutree(HClust, k=groups) == j) %>% Labs[.]})

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
      "The variables are divided the following two/three groups: \n"
    )
    print(trn)

    ggD3 <- argTREC$ggD3

    ran <- range(ggD3$t)

    TRN <- lapply(1:groups, function(j){
      cbind(trn = paste0("trn", j), V = trn[[j]])
    }) %>% do.call(rbind, .) %>% data.frame %>%
      mutate(V = factor(V, levels=Labs))

    Labs1 <- paste(TRN$trn, TRN$V, sep="-")

    TRN1 <- mutate(TRN, V1 = Labs1) %>%
      right_join(ggD3, by="V") %>%
      select(V0=V, V=V1, everything()) %>%
      mutate(V = factor(V, levels=Labs1))

    fig.trends <- subplot(
      lapply(1:groups, function(j){
        fig <- subset(TRN1, trn==paste0("trn", j)) %>%
          ggplot() +
          geom_line(aes(x=x, y=t, col=V)) +
          theme(
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          ) +
          ylim(ran) +
          facet_wrap(~trn)

        if(j != 1)
        {
          fig <- fig + theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        }

        return(fig)
      })
    )

    # fig.trends <- plotly::ggplotly(
    #   mutate(TRN, V1 = Labs1) %>%
    #     right_join(ggD3, by="V") %>%
    #     select(V0=V, V=V1, everything()) %>%
    #     mutate(V = factor(V, levels=Labs1)) %>%
    #     ggplot() +
    #     geom_line(aes(x=x, y=t, col=V)) +
    #     theme(
    #       axis.title = element_blank(),
    #       axis.text.x = element_blank(),
    #       axis.ticks.x = element_blank()
    #     ) +
    #     ylim(ran) +
    #     facet_wrap(~trn)
    # )

    print(fig.trends)

    # fig.trend1 <- ggD3 %>% subset(V %in% trn[[1]]) %>% ggplot() +
    #   geom_line(aes(x=x, y=t, col=V)) +
    #   theme(
    #     axis.title = element_blank(),
    #     axis.text.x = element_blank(),
    #     axis.ticks.x = element_blank()
    #   ) +
    #   ylim(ran)
    #
    # fig.trend2 <- ggD3 %>% subset(V %in% trn[[2]]) %>% ggplot() +
    #   geom_line(aes(x=x, y=t, col=V)) +
    #   theme(
    #     axis.title = element_blank(),
    #     axis.text.x = element_blank(),
    #     axis.ticks.x = element_blank()
    #   ) +
    #   ylim(ran)
    #
    # if(groups == 2)
    # {
    #   fig.trends <- grid.arrange(fig.trend1, fig.trend2, ncol=2)
    # } else if(groups == 3)
    # {
    #   fig.trend3 <- ggD3 %>% subset(V %in% trn[[3]]) %>% ggplot() +
    #     geom_line(aes(x=x, y=t, col=V)) +
    #     theme(
    #       axis.title = element_blank(),
    #       axis.text.x = element_blank(),
    #       axis.ticks.x = element_blank()
    #     ) +
    #     ylim(ran)
    #
    #   fig.trends <- grid.arrange(fig.trend1, fig.trend2, fig.trend3, ncol=3)
    # }

    ans1 <- ""
    while(!ans1 %in% c("yes", "no"))
    {
      ans1 <- readline("Do you need some modifications? Please enter yes or no: ")
    }

    if(ans1 == "no")
    {
      Out[[2]] <- fig.trends
      names(Out)[2] <- "fig.trends"

      cat("Select tvar and proceed TREC3.\n")
      cat("You have 'trn' object for TREC3.\n")
    } else
    {
      trn1 <<- trn[[1]]
      trn2 <<- trn[[2]]

      if(groups==2)
      {
        cat("You can use three objects 'trn', 'trn1', and 'trn2' to modify the groups.\n")
        cat("Redefine 'trn' and execute TREC2.1.\n")
      } else
      {
        trn3 <<- trn[[3]]

        cat("You can use three objects 'trn', 'trn1', 'trn2', and 'trn3' to modify the groups.\n")
        cat("Redefine 'trn' and execute TREC2.1.\n")
      }
    }

  } else
  {
    cat("trec procedure terminates.\n")
    cat("You have variables for each group as 'trn' object.\n")
  }

  return(Out)
}

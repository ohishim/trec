#' @title The third step of TREC
#' @description \code{TREC3} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 annotation_raster
#' @importFrom ggplot2 ylim
#' @importFrom tidyr gather
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom png readPNG
#' @importFrom gridExtra grid.arrange
#' @importFrom gt gt
#' @importFrom gt text_transform
#' @importFrom gt cells_body
#' @importFrom gt local_image
#' @importFrom gt px
#' @importFrom gt ggplot_image
#' @importFrom gt tab_style
#' @importFrom gt cell_text
#' @importFrom gt cells_column_labels
#' @param tvar a vector of variable names for target trends.
#' @param argTREC the output "argTREC" of TREC1.
#' @return some figures for trends and assigned icons
#' @export
#' @examples
#' #TREC3(tnum, argTREC)

TREC3 <- function(tvar, argTREC){

  ggD3 <- argTREC$ggD3

  tr.n <- length(tvar)
  split <- tr.n > 16

  ##############################################################################
  ###   Set target trend corresponding to icon
  ##############################################################################

  #=============================================================================
  ###   clustering based on the target trends
  #=============================================================================

  TR <- argTREC$TR
  Labs <- colnames(TR)
  p <- ncol(TR)

  tnum <- which(Labs %in% tvar)
  tvar <- Labs[tnum]

  if(split)
  {
    div.n <- ceiling(tr.n / 16)

    if(tr.n %% div.n == 0)
    {
      div <- split(
        tvar,
        rep(1:div.n, each=tr.n / div.n)
      )
    } else
    {
      div <- split(
        tvar,
        c(
          rep(1:div.n, tr.n %/% div.n),
          1:(tr.n %% div.n)
        ) %>% sort
      )
    }
  }

  TGTR <- TR[,tnum]

  L <- sapply(1:p, function(j){
    if(j %in% tnum)
    {
      out <- j
    } else
    {
      tj <- TR[,j]
      out <- (TGTR - tj) %>% apply(2, function(x){sum(x^2)}) %>% which.min %>% tnum[.]
    }
    return(out)
  }) %>% Labs[.]

  ggD4 <- data.frame(
    V = ggD3$V %>% unique,
    L = factor(L, levels=Labs)
  ) %>% left_join(ggD3, ., by="V")

  if(split)
  {
    fig.tgtrend.G <- lapply(1:div.n, function(j){
      subset(ggD4, L %in% div[[j]]) %>% ggplot() +
        geom_line(aes(x=x, y=t, col=V)) +
        facet_wrap(.~L) +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    })
  } else
  {
    fig.tgtrend.G <- ggplot(ggD4) +
      geom_line(aes(x=x, y=t, col=V)) +
      facet_wrap(.~L) +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }

  #=============================================================================
  ###   the target trend plots
  #=============================================================================

  if(split)
  {
    fig.tgtrend <- lapply(1:div.n, function(j){
      subset(ggD4, V %in% div[[j]]) %>% subset(V %in% tvar) %>%
        ggplot() +
        geom_line(aes(x=x, y=t)) +
        facet_wrap(.~V) +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    })
  } else
  {
    fig.tgtrend <- ggD4 %>% subset(V %in% tvar) %>%
      ggplot() +
      geom_line(aes(x=x, y=t)) +
      facet_wrap(.~V) +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }

  ##############################################################################
  ###   Assign icon to the target trends
  ##############################################################################

  Y <- argTREC$Y

  res <- data.frame(
    V = colnames(Y[,tnum]),
    icon = cbind(1, argTREC$dim, argTREC$coef)[tnum,] %>%  apply(1, icon.fit),
    argTREC$TR[,tnum] %>% t
  )

  n <- ncol(res) - 2
  p <- nrow(res)
  ggD <- gather(res, 3:(n+2), key="t", value="y") %>%
    mutate(
      x = seq(0, 1, length=n) %>% rep(each=p),
      V = factor(V, levels=unique(V))
    )

  yr <- ggD$y %>% range

  tvar.n <- length(tvar)

  ttrends <- lapply(1:tvar.n, function(j){
    ggDj <- subset(ggD, V==tvar[j])

    figj <- ggplot() +
      geom_line(data=ggDj, aes(x=x, y=y), size=2) +
      theme(
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text =  element_blank(),
        axis.ticks = element_blank()
      ) +
      ylim(yr)
  })

  paths <- paste0("icon", res$icon, ".png") %>%
    sapply(function(x){system.file(x, package="trec")})

  fig.icon <- data.frame(
    tvar = tvar,
    trend = 1:tvar.n,
    icon = 1:tvar.n
  ) %>% gt %>%
    text_transform(
      locations = cells_body(
        columns = icon
      ),
      fn = function(x) {
        local_image(
          filename = paths,
          height = px(50)
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = trend
      ),
      fn = function(x) {
        ggplot_image(ttrends, height = px(80))
      }
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels()
    )

  print(fig.icon)

  # figs <- lapply(1:tvar.n, function(j){
  #   ggDj <- subset(ggD, V==tvar[j])
  #
  #   figj <- ggplot() +
  #     geom_line(data=ggDj, aes(x=x, y=y)) +
  #     facet_wrap(.~V) +
  #     annotation_raster(
  #       ImagesOfIcons[[res$icon[j]]],
  #       xmin = -Inf, xmax = 0.15, ymax = Inf,
  #       ymin = yr[2] - (3*(yr[2] - yr[1])/10)
  #     ) +
  #     theme(
  #       axis.title = element_blank(),
  #       legend.title = element_blank(),
  #       axis.text.x =  element_blank(),
  #       axis.ticks.x = element_blank()
  #     ) +
  #     ylim(yr)
  # })
  #
  # if(split)
  # {
  #   div1 <- split(
  #     1:tr.n,
  #     lapply(1:div.n, function(j){rep(j, length(div[[j]]))}) %>% unlist
  #   )
  #
  #   fig.icon <- lapply(1:div.n, function(j){
  #
  #     idx <- div1[[j]]
  #     idx.n <- length(idx)
  #
  #     if(idx.n <= 3)
  #     {
  #       fig.col <- idx.n
  #     } else if(idx.n == 4)
  #     {
  #       fig.col <- 2
  #     } else
  #     {
  #       fig.col <- 3
  #     }
  #
  #     fig.icon <- do.call(
  #       grid.arrange,
  #       c(figs[idx], list(ncol=fig.col))
  #     )
  #   })
  # } else
  # {
  #   if(tvar.n <= 3)
  #   {
  #     fig.col <- tvar.n
  #   } else if(tvar.n == 4)
  #   {
  #     fig.col <- 2
  #   } else
  #   {
  #     fig.col <- 3
  #   }
  #
  #   fig.icon <- do.call(
  #     grid.arrange,
  #     c(figs, list(ncol=fig.col))
  #   )
  # }

  ##############################################################################
  ###   Output
  ##############################################################################

  Out <- list(
    fig.tgtrend.G = fig.tgtrend.G,
    fig.tgtrend = fig.tgtrend,
    fig.icon = fig.icon,
    group = split(Labs, factor(L, levels=tvar))
  )

  cat("variables for each group: \n")
  print(Out$group)

  return(Out)
}

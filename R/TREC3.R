#' @title The third step of TREC
#' @description \code{TREC3} This function performs clustering for trends.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr set_names
#' @importFrom magrittr extract
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
#' @importFrom dplyr arrange
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
#' @importFrom gt tab_stubhead
#' @param tvar a vector of variable names for target trends.
#' @param trn the output `trn` of `TREC2`.
#' @param argTREC the output `argTREC` of `TREC1`.
#' @return a list of concrete classification results which has the following elements:
#' \item{fig.down}{a figure of estimated trends for each group in "Downward";
#'     if the number of variables of downward group is larger than 16, this is a list of figures}
#'
#' \item{fig.up}{a figure of estimated trends for each group in "Upward";
#'     if the number of variables of upward group is larger than 16, this is a list of figures}
#'
#' \item{fig.flat}{a figure of estimated trends for each group in "Flat";
#'     if the number of variables of flat group is larger than 16, this is a list of figures}
#'
#' \item{fig.icon}{summary}
#' @export
#' @examples
#' #TREC3(tvar, trn, argTREC)

TREC3 <- function(tvar, trn, argTREC){

  trn <- which(names(trn) %in% names(tvar)) %>% trn[.]

  groups <- length(trn)

  tvar <- lapply(1:groups, function(j){
    trn[[j]][
      trn[[j]] %in% tvar[[j]]
    ] %>% factor(., levels=.)
  }) %>% set_names(names(trn))

  trn <- lapply(trn, function(x){factor(x, levels=x)})

  ggD3 <- argTREC$ggD3

  tr.n <- unlist(tvar) %>% length

  ##############################################################################
  ###   Set target trend corresponding to icon
  ##############################################################################

  #=============================================================================
  ###   clustering based on the target trends
  #=============================================================================

  TR <- argTREC$TR
  Labs <- colnames(TR)
  L <- list()

  for(j in 1:groups)
  {
    tvarj <- tvar[[j]]

    if(length(tvarj) == 1)
    {
      L[[j]] <- rep(tvarj, length(trn[[j]]))
    } else
    {
      TRj <- which(Labs %in% trn[[j]]) %>% TR[,.]
      Labsj <- trn[[j]]
      pj <- length(Labsj)

      tnumj <- which(Labsj %in% tvarj)

      TGTRj <- TRj[,tnumj]

      L[[j]] <- sapply(1:pj, function(l){
        if(l %in% tnumj)
        {
          out <- l
        } else
        {
          out <- (TGTRj - TRj[,l]) %>% apply(2, function(x){sum(x^2)}) %>% which.min %>% tnumj[.]
        }
        return(out)
      }) %>% Labsj[.] %>% factor(levels=tvarj)
    }
  } #end for

  #=============================================================================
  ###   the target trend plots
  #=============================================================================

  fig.down <- fig.up <- fig.flat <- NULL

  for(j in 1:groups)
  {
    tr.nj <- length(tvar[[j]])
    split <- tr.nj > 16

    ggD4 <- data.frame(
      V = trn[[j]],
      L = L[[j]]
    ) %>% left_join(ggD3, by="V")

    if(split)
    {
      div.n <- ceiling(tr.nj / 16)

      if(tr.nj %% div.n == 0)
      {
        div <- split(
          tvar[[j]],
          rep(1:div.n, each=tr.nj / div.n)
        )
      } else
      {
        div <- split(
          tvar[[j]],
          c(
            rep(1:div.n, tr.nj %/% div.n),
            1:(tr.nj %% div.n)
          ) %>% sort
        )
      }

      fig <- lapply(1:div.n, function(j){
        subset(ggD4, L %in% div[[j]]) %>% ggplot() +
          geom_line(aes(x=x, y=t, col=V)) +
          facet_wrap(.~L) +
          theme(
            axis.title = element_blank()
          )
      })
    } else
    {
      fig <- ggplot(ggD4) +
        geom_line(aes(x=x, y=t, col=V)) +
        facet_wrap(~L) +
        theme(
          axis.title = element_blank()
        )
    }

    if(names(trn)[[j]] == "Downward")
    {
      fig.down <- fig
    }

    if(names(trn)[[j]] == "Upward")
    {
      fig.up <- fig
    }

    if(names(trn)[[j]] == "Flat")
    {
      fig.flat <- fig
    }
  } #end for

  ##############################################################################
  ###   Assign icon to the target trends
  ##############################################################################

  ttrends <- paths <- group <- list()
  yr <- ggD3$t %>% range

  for(j in 1:groups)
  {
    tnumj <- which(Labs %in% tvar[[j]])

    res <- data.frame(
      V = tvar[[j]],
      g = names(trn)[[j]],
      icon = cbind(1, argTREC$dim, argTREC$coef)[tnumj,,drop=F] %>% apply(1, function(x){
        icon.fit(x, names(trn)[j])
      }),
      argTREC$TR[,tnumj] %>% t
    )

    n <- ncol(res) - 3
    p <- nrow(res)
    ggD <- gather(res, 4:(n+3), key="t", value="y") %>%
      mutate(
        x = seq(0, 1, length=n) %>% rep(each=p)
      )

    tvar.n <- length(tnumj)

    ttrends[[j]] <- lapply(1:tvar.n, function(l){
      ggDj <- subset(ggD, V==tvar[[j]][l])

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

    paths[[j]] <- paste0("icon", res$icon, ".png") %>%
      sapply(function(x){system.file(x, package="trec")})

    group[[j]] <- split(trn[[j]], L[[j]])
  } #end for

  fig.icon <- data.frame(
    tvar = unlist(tvar),
    g = lapply(1:groups, function(j){rep(names(trn)[[j]], length(tvar[[j]]))}) %>% unlist,
    group = do.call(c, group) %>% sapply(function(x){paste(x, collapse=", ")}),
    trend = 1,
    icon = 1
  ) %>% gt(rowname_col = "tvar", groupname_col = "g") %>%
    text_transform(
      locations = cells_body(
        columns = icon
      ),
      fn = function(x) {
        local_image(
          filename = unlist(paths),
          height = px(50)
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = trend
      ),
      fn = function(x) {
        ggplot_image(do.call(c, ttrends), height = px(80))
      }
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = c("trend", "icon"))
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels()
    ) %>%
    tab_stubhead("tvar")

  print(fig.icon)

  ##############################################################################
  ###   Output
  ##############################################################################

  Out <- list(
    fig.down = fig.down,
    fig.up = fig.up,
    fig.flat = fig.flat,
    fig.icon = fig.icon
  )

  return(Out)
}

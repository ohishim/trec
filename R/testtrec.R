#' @title test
#' @description \code{testtrec} This is test function.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 annotation_raster
#' @importFrom ggplot2 ylim
#' @importFrom gt gt
#' @importFrom gt text_transform
#' @importFrom gt cells_body
#' @importFrom gt local_image
#' @importFrom gt ggplot_image
#' @importFrom gt px
#' @param v a number
#' @return some figures for trends and assigned icons
#' @export
#' @examples
#' #testtrec(v)

testtrec <- function(v){

  fig <- data.frame(
    x = 1:10,
    y = (1:10)^2
  ) %>% ggplot() +
    geom_line(aes(x=x, y=y)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

  # path <- paste0("icon", v, ".png")
  path <- paste0("icon", v, ".png") %>%
    system.file(package="trec")

  out <- data.frame(
    v = v,
    tr = NA,
    icon = NA
  ) %>% gt() %>%
    text_transform(
      locations = cells_body(columns = icon),
      fn = function(x) {
        local_image(
          filename = path,
          height = as.numeric(50)
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = tr),
      fn = function(x) {
          ggplot_image(fig, height = px(100))
      }
    )

  print(out)
}

#' Custom ggplot2 theme
#'
#' @description A custom ggplot2 theme. For some inspiration,
#' see https://github.com/Yu-Group/vthemes/blob/main/R/ggplot-themes.R
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  theme_custom()
#'
#' @export
theme_custom <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial", size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black")
    )
}

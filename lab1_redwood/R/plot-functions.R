#' Helper function to plot redwood variables over time
#'
#' @param data the redwood data (wide formatted)
#' @param x the x variable
#' @param y the y variable
#' @param color the color variable
#' @param variables the variables to plot
#' @param size the size of the points
#' @param ... additional arguments to pass to ggplot2::geom_point
plot_redwood_over_time <- function(data,
                                   x = "datetime", y = "value",
                                   color = NULL,
                                   variables = c("humidity", "temp", "iPAR", "rPAR"),
                                   size = 0.1,
                                   ...) {

  data_long <- data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(variables),
      names_to = "variable",
      values_to = "value"
    )

  if (is.null(color)) {
    plt <- data_long |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y)
      )
  } else {
    plt <- data_long |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        color = !!rlang::sym(color)
      )
  }

  plt <- plt +
    ggplot2::facet_grid(variable ~ ., scales = "free_y") +
    ggplot2::geom_point(
      size = size,
      ...
    )
  return(plt)
}



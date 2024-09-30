#' Plot linguistic survey results on US map
#'
#' @param ling_data the linguistic survey data
#' @param qa_key the question and answer key
#' @param qid the question ID
#' @param point_size the size of the points on the map
#' @param map_type the type of map to plot
#' @param show_contiguous_us whether to show only the contiguous US
#'
#' @returns a ggplot object
plot_ling_map <- function(ling_data, qa_key, qid, point_size = 0.5,
                          map_type = c("state", "county"),
                          show_contiguous_us = TRUE) {
  map_type <- match.arg(map_type)
  qcol <- dplyr::case_when(
    qid < 100 ~ paste0("Q0", qid),
    TRUE ~ paste0("Q", qid)
  )
  merge_by <- "answer_num"
  names(merge_by) <- qcol

  qid_key <- qa_key |>
    dplyr::filter(qid == !!qid)
  plt_df <- ling_data |>
    dplyr::left_join(qid_key, by = merge_by)

  if (show_contiguous_us) {
    plt_df <- plt_df |>
      dplyr::filter(!(ZIP_state_abb %in% c("HI", "AK")))
  }
  map_df <- ggplot2::map_data(map_type)
  plt <- plt_df |>
    ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long, y = lat, group = group),
      data = map_df, color = "black", fill = NA
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = ZIP_long, y = ZIP_lat, color = answer),
      size = point_size
    ) +
    ggplot2::labs(
      title = sprintf("%s: %s", qcol, qid_key$question[[1]]),
      color = "Answer"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 3))
    )
  return(plt)
}

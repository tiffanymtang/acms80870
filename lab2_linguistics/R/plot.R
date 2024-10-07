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

  # get question ID
  qcol <- dplyr::case_when(
    qid < 100 ~ paste0("Q0", qid),
    TRUE ~ paste0("Q", qid)
  )
  merge_by <- "answer_num"
  names(merge_by) <- qcol

  # get responses for the question of interest
  qid_key <- qa_key |>
    dplyr::filter(qid == !!qid)

  # merge text responses with the survey data
  plt_df <- ling_data |>
    dplyr::left_join(qid_key, by = merge_by)

  if (show_contiguous_us) {
    # remove Hawaii and Alaska for plotting purposes
    plt_df <- plt_df |>
      dplyr::filter(!(ZIP_state_abb %in% c("HI", "AK")))
  }

  # get state/county boundaries data
  map_df <- ggplot2::map_data(map_type)

  # make map plot
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
    ) +
    # scale it to make it look more like a map
    ggplot2::coord_fixed(1.5)

  return(plt)
}


#' Plot linguistic survey results, aggregated by county
#'
#' @param ling_data the linguistic survey data
#' @param qa_key the question and answer key
#' @param qid the question ID
#' @param linewidth the width of the county borders
#' @param show_contiguous_us whether to show only the contiguous US
#'
#' @returns a ggplot object
plot_ling_map_by_county <- function(ling_data, qa_key, qid, linewidth = 0.1,
                                    show_contiguous_us = TRUE) {

  # get county boundaries data
  map_df <- ggplot2::map_data("county")

  # get question ID
  qcol <- dplyr::case_when(
    qid < 100 ~ paste0("Q0", qid),
    TRUE ~ paste0("Q", qid)
  )
  merge_by <- "answer_num"
  names(merge_by) <- qcol

  if (show_contiguous_us) {
    # remove Hawaii and Alaska for plotting purposes
    ling_data <- ling_data |>
      dplyr::filter(!(ZIP_state_abb %in% c("HI", "AK")))
  }

  # get responses for the question of interest
  qid_key <- qa_key |>
    dplyr::filter(qid == !!qid)

  # prepare data for plotting
  plt_df <- ling_data |>
    dplyr::left_join(qid_key, by = merge_by) |>
    # do some cleaning of the counties to match the map data
    dplyr::mutate(
      ZIP_county = dplyr::case_when(
        ZIP_state == "louisiana" ~ stringr::str_remove(ZIP_county, " parish"),
        ZIP_county == "dekalb" ~ "de kalb",
        ZIP_county == "desoto" ~ "de soto",
        (ZIP_state == "virginia") &
          !(ZIP_county %in% c("james city", "charles city")) ~
          stringr::str_remove(ZIP_county, " city"),
        ZIP_county == "dewitt" ~ "de witt",
        ZIP_county == "dupage" ~ "du page",
        ZIP_county == "lamoure" ~ "la moure",
        ZIP_county == "laporte" ~ "la porte",
        ZIP_county == "lasalle" ~ "la salle",
        ZIP_county == "district of columbia" ~ "washington",
        TRUE ~ stringr::str_remove_all(ZIP_county, "'")
      ),
      ZIP_state = dplyr::case_when(
        ZIP_state == "dc" ~ "district of columbia",
        TRUE ~ ZIP_state
      )
    ) |>
    dplyr::filter(!is.na(answer)) |>
    # aggregate response by county
    dplyr::group_by(ZIP_county, ZIP_state) |>
    dplyr::summarise(
      answer = names(which.max(table(answer))[1]),
      ZIP_lat = mean(ZIP_lat),
      ZIP_long = mean(ZIP_long),
      .groups = "drop"
    ) |>
    # merge with county boundaries data
    dplyr::left_join(
      map_df, by = c("ZIP_county" = "subregion", "ZIP_state" = "region")
    )

  # look at county mismatches in merge
  # plt_df |>
  #   dplyr::filter(is.na(long)) |>
  #   dplyr::distinct(ZIP_county, ZIP_state)

  # make map plot
  plt <- plt_df |>
    ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long, y = lat, group = group, fill = answer),
      data = plt_df, color = "black", linewidth = linewidth
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long, y = lat, group = group),
      data = map_df, color = "black", linewidth = linewidth, fill = NA
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
    ) +
    # scale it to make it look more like a map
    ggplot2::coord_fixed(1.5)
  return(plt)
}


#' Plot PCA results
#'
#' @param pca_out the PCA output
#' @param npcs the number of principal components to plot
#' @param color a vector to use for coloring data points
#' @param point_size the size of the points
#' @param point_alpha the transparency of the points
#' @param ... additional arguments to pass to vdocs::plot_pairs()
#'
#' @returns a ggplot object
plot_pca <- function(pca_out, npcs = 2, color = NULL,
                     point_size = 0.5, point_alpha = 0.5, ...) {
  # get the data to plot
  pca_df <- as.data.frame(pca_out$x)
  colnames(pca_df) <- sprintf(
    "%s (PVE = %.2f)", colnames(pca_df), pca_out$sdev
  )

  # make the plot
  if (npcs == 2) {
    if (is.null(color)) {
      plt <- ggplot2::ggplot(pca_df) +
        ggplot2::aes(
          x = .data[[colnames(pca_df)[1]]],
          y = .data[[colnames(pca_df)[2]]],
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha) +
        vthemes::theme_vmodern(...)
    } else {
      plt <- pca_df |>
        dplyr::mutate(
          .color = color
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .data[[colnames(pca_df)[1]]],
          y = .data[[colnames(pca_df)[2]]],
          color = .color
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha) +
        vthemes::scale_color_vmodern(discrete = is.factor(color)) +
        vthemes::theme_vmodern(...)
    }
  } else {
    plt <- vdocs::plot_pairs(
      pca_df, columns = 1:npcs, color = color,
      point_size = point_size, point_alpha = point_alpha, ...
    )
  }
  return(plt)
}


#' Plot dimension reduction results
#'
#' @param X the data matrix
#' @param ndim the number of top components to plot
#' @param color a vector to use for coloring data points
#' @param point_size the size of the points
#' @param point_alpha the transparency of the points
#' @param ... additional arguments to pass to vdocs::plot_pairs()
#'
#' @returns a ggplot object
plot_dr_scatter <- function(X, ndim = 2, color = NULL,
                            point_size = 0.5, point_alpha = 0.5, ...) {
  # get the data to plot
  plt_df <- as.data.frame(X)

  # make the plot
  if (ndim == 2) {
    if (is.null(color)) {
      plt <- ggplot2::ggplot(plt_df) +
        ggplot2::aes(
          x = .data[[colnames(plt_df)[1]]],
          y = .data[[colnames(plt_df)[2]]],
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha) +
        vthemes::theme_vmodern(...)
    } else {
      plt <- plt_df |>
        dplyr::mutate(
          .color = color
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .data[[colnames(plt_df)[1]]],
          y = .data[[colnames(plt_df)[2]]],
          color = .color
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha) +
        vthemes::scale_color_vmodern(discrete = is.factor(color)) +
        vthemes::theme_vmodern(...)
    }
  } else {
    plt <- vdocs::plot_pairs(
      plt_df, columns = 1:ndim, color = color,
      point_size = point_size, point_alpha = point_alpha, ...
    )
  }
  return(plt)
}


#' Plot dimension reduction results on a map
#'
#' @param X the data matrix
#' @param ling_data the linguistic data
#' @param ndim the number of top components to plot
#' @param point_size the size of the points
#' @param point_alpha the transparency of the points
#' @param linewidth the width of the county borders
#' @param by_county whether to plot by county
#' @param show_contiguous_us whether to show only the contiguous US
#'
#' @returns a ggplot object
plot_dr_map <- function(X, ling_data, ndim = 2,
                        point_size = 0.5, point_alpha = 0.5, linewidth = 0.1,
                        by_county = FALSE, show_contiguous_us = TRUE) {
  # get state/county boundaries data
  map_type <- ifelse(by_county ,"county", "state")
  map_df <- ggplot2::map_data(map_type)
  state_map_df <- ggplot2::map_data("state")

  # get the data to plot
  X_df <- as.data.frame(X)
  plt_df <- ling_data |>
    dplyr::select(tidyselect::starts_with("ZIP")) |>
    dplyr::bind_cols(X_df)
  if (show_contiguous_us) {
    # remove Hawaii and Alaska for plotting purposes
    plt_df <- plt_df |>
      dplyr::filter(!(ZIP_state_abb %in% c("HI", "AK")))
  }
  if (by_county) {
    plt_df <- plt_df |>
      # do some cleaning of the counties to match the map data
      dplyr::mutate(
        ZIP_county = dplyr::case_when(
          ZIP_state == "louisiana" ~ stringr::str_remove(ZIP_county, " parish"),
          ZIP_county == "dekalb" ~ "de kalb",
          ZIP_county == "desoto" ~ "de soto",
          (ZIP_state == "virginia") &
            !(ZIP_county %in% c("james city", "charles city")) ~
            stringr::str_remove(ZIP_county, " city"),
          ZIP_county == "dewitt" ~ "de witt",
          ZIP_county == "dupage" ~ "du page",
          ZIP_county == "lamoure" ~ "la moure",
          ZIP_county == "laporte" ~ "la porte",
          ZIP_county == "lasalle" ~ "la salle",
          ZIP_county == "district of columbia" ~ "washington",
          TRUE ~ stringr::str_remove_all(ZIP_county, "'")
        ),
        ZIP_state = dplyr::case_when(
          ZIP_state == "dc" ~ "district of columbia",
          TRUE ~ ZIP_state
        )
      ) |>
      # aggregate results per county
      dplyr::group_by(ZIP_county, ZIP_state) |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::all_of(colnames(X_df)), mean
        ),
        .groups = "drop"
      ) |>
      # merge with county boundaries data
      dplyr::left_join(
        map_df, by = c("ZIP_county" = "subregion", "ZIP_state" = "region")
      )
  }

  plt_ls <- list()
  for (i in 1:ndim) {
    # make map plot
    if (by_county) {
      plt_ls[[i]] <- plt_df |>
        ggplot2::ggplot() +
        ggplot2::geom_polygon(
          ggplot2::aes(
            x = long, y = lat, group = group,
            fill = .data[[colnames(X_df)[i]]],
            color = .data[[colnames(X_df)[i]]]
          ),
          data = plt_df, linewidth = linewidth
        ) +
        ggplot2::geom_polygon(
          ggplot2::aes(x = long, y = lat, group = group),
          data = state_map_df,
          color = "black", linewidth = linewidth * 2, fill = NA
        )
    } else {
      plt_ls[[i]] <- plt_df |>
        ggplot2::ggplot() +
        ggplot2::geom_polygon(
          ggplot2::aes(x = long, y = lat, group = group),
          data = map_df, color = "black", fill = NA
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = ZIP_long, y = ZIP_lat, color = .data[[colnames(X_df)[i]]]
          ),
          size = point_size
        )
    }
    plt_ls[[i]] <- plt_ls[[i]] +
      ggplot2::labs(
        title = sprintf("Component %s", i),
        color = "Value", fill = "Value"
      ) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      ) +
      vthemes::scale_color_vmodern(discrete = FALSE) +
      vthemes::scale_fill_vmodern(discrete = FALSE) +
      # scale it to make it look more like a map
      ggplot2::coord_fixed(1.5)
  }
  plt <- patchwork::wrap_plots(plt_ls)
  return(plt)
}

#' Clean cloud data
#'
#' @param data list of 3 data frames of the 3 cloud images data
#' @param unsure_class class to assign to unsure labels (default: "No Clouds")
#'
#' @return a list of 3 cleaned data frames of the 3 cloud images data
clean_cloud_data <- function(data, unsure_class = c("No Clouds", "Clouds")) {
  unsure_class <- match.arg(unsure_class)

  # informative column names
  cnames <- c(
    'y', 'x', 'label', 'NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN'
  )

  # clean each cloud image
  cloud_data_ls <- purrr::map(
    data,
    function(cloud_data) {
      cloud_data <- cloud_data |>
        setNames(cnames) |>
        dplyr::mutate(
          label = factor(label, levels = c("-1", "1", "0")) |>
            forcats::fct_recode(
              "No Clouds" = "-1",
              "Unsure" = "0",
              "Clouds" = "1"
            )
        )
      if (unsure_class == "No Clouds") {
        cloud_data <- cloud_data |>
          dplyr::mutate(
            binary_label = forcats::fct_recode(label, "No Clouds" = "Unsure")
          )
      } else {
        cloud_data <- cloud_data |>
          dplyr::mutate(
            binary_label = forcats::fct_recode(label, "Clouds" = "Unsure")
          )
      }
      return(cloud_data)
    }
  )

  return(cloud_data_ls)
}


#' Split cloud data into blocks
#'
#' @description This function divides the cloud images into blocks so as to not
#'   destroy the image structure when splitting data into training/test sets
#'
#' @param data list of 3 cleaned data frames of the 3 cloud images data
#' @param n_blocks_x number of blocks to divide width of image into
#' @param n_blocks_y number of blocks to divide height of image into
#'
#' @returns a data frame with all three cloud images and two additional columns
#'   "image" giving the image number and "block_id" giving the block id number
add_cloud_blocks <- function(data, n_blocks_x = 2, n_blocks_y = 2) {

  # divide cloud data into blocks
  cloud_data_df <- purrr::map(
    data,
    function(cloud_data) {
      block_size_x <- ceiling(
        (max(cloud_data$x) - min(cloud_data$x) + 1) / n_blocks_x
      )
      block_size_y <- ceiling(
        (max(cloud_data$y) - min(cloud_data$y) + 1) / n_blocks_y
      )
      cloud_data <- cloud_data |>
        dplyr::mutate(
          x_block_id = (x - min(x)) %/% block_size_x,
          y_block_id = (y - min(y)) %/% block_size_y
        )
      return(cloud_data)
    }
  ) |>
    dplyr::bind_rows(.id = "image")

  # get cloud block ids
  block_ids <- cloud_data_df |>
    dplyr::group_by(image, x_block_id, y_block_id) |>
    dplyr::group_indices()

  # add cloud block ids to data frame
  cloud_data_df <- cloud_data_df |>
    dplyr::mutate(
      block_id = as.character(block_ids)
    ) |>
    dplyr::select(-x_block_id, -y_block_id)

  return(cloud_data_df)
}

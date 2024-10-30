#' Load cloud data
#'
#' @param path directory where cloud images are stored
#'
#' @return a list of 3 data frames of the 3 cloud images data
load_cloud_data <- function(path = here::here("data")) {
  image1 <- data.table::fread(file.path(path, 'image1.txt'), data.table = FALSE)
  image2 <- data.table::fread(file.path(path, 'image2.txt'), data.table = FALSE)
  image3 <- data.table::fread(file.path(path, 'image3.txt'), data.table = FALSE)
  clouds <- list(image1, image2, image3) |>
    purrr::map(~ tibble::as_tibble(.x))
  return(clouds)
}

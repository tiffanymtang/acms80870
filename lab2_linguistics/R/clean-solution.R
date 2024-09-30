#' Merge linguistics data with ZIP location data
#'
#' @param ling_data the linguistics data
#' @param zip_data the ZIP data
#'
#' @returns A merged data frame
merge_ling_zip_data <- function(ling_data, zip_data) {
  merged_data <- ling_data |>
    dplyr::mutate(
      ZIP = as.character(ZIP)
    ) |>
    dplyr::left_join(zip_data, by = "ZIP")
  return(merged_data)
}


#' Clean linguistics data
#'
#' @param ling_data the merged linguistics data
#'
#' @returns A cleaned data frame
clean_ling_data <- function(ling_data) {
  cleaned_data <- ling_data #|>  # ADD IN ANY OTHER DESIRED CLEANING STEPS HERE
  return(cleaned_data)
}

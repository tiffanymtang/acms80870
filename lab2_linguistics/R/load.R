#' Load in question and answer key
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the question and answer key
load_q_and_a_key <- function(path = here::here("data")) {
  data.table::fread(
    file.path(path, "q_and_a_key.csv")
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      question = as.factor(question)
    )
}


#' Load in lingusitics data
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the linguistics survey data
load_ling_data <- function(path = here::here("data")) {
  # TO FILL IN
}


#' Load in ZIP locations data
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the ZIP locations data
load_zip_data <- function(path = here::here("data")) {
  data.table::fread(
    file.path(path, "zip_locations.csv"),
    colClasses = list(character = c("ZIP"))
  ) |>
    tibble::as_tibble()
}

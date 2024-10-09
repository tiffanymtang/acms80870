#' Load in question and answer key
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the question and answer key
#' @export
load_q_and_a_key <- function() {
  return(qa_key)
}


#' Load in lingusitics data
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the linguistics survey data
#' @export
load_ling_data <- function() {
  return(ling_data)
}


#' Load in ZIP locations data
#'
#' @param path the path to the data
#'
#' @returns A data frame containing the ZIP locations data
#' @export
load_zip_data <- function(path = here::here("data")) {
  return(zip_data)
}

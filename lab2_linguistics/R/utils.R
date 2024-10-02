#' Get a numeric X matrix from the linguistic data
#'
#' @param ling_data the linguistic data
#'
#' @returns a numeric matrix
get_X_matrix <- function(ling_data) {
  ling_data |>
    dplyr::select(tidyselect::starts_with("Q")) |>
    as.matrix()
}


#' Convert numeric answers to text
#'
#' @param x the numeric answers
#' @param qid the question ID
#' @param qa_key the question-answer key
#'
#' @returns a factor with text answers
get_answers <- function(x, qid, qa_key) {
  qid_key <- qa_key |>
    dplyr::filter(qid == !!qid)
  factor(
    x,
    levels = qid_key$answer_num,
    labels = qid_key$answer
  )
}

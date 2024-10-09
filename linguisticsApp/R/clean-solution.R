#' Merge linguistics data with ZIP location data
#'
#' @param ling_data the linguistics data
#' @param zip_data the ZIP data
#'
#' @returns A merged data frame
#' @export
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
#' @export
clean_ling_data <- function(ling_data, qa_key = NULL) {
  cleaned_data <- ling_data #|>  # ADD IN ANY OTHER DESIRED CLEANING STEPS HERE
  return(cleaned_data)
}


#' One-hot encoding of categorical variables
#'
#' @param ling_data the linguistics data
#' @param return_matrix whether to return a matrix including only the survey
#'   responses or the full data frame (including location information)
#' @param remove_zeros whether to remove .0 columns which correspond to NAs
#'
#' @returns A one-hot-encoded data matrix
#' @export
one_hot_ling_data <- function(ling_data,
                              remove_zeros = FALSE,
                              return_matrix = TRUE) {
  # do one hot encoding of survey responses
  require("caret")
  X <- ling_data |>
    dplyr::select(tidyselect::starts_with("Q")) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.factor)
    )
  dummy_fit <- caret::dummyVars(~ ., data = X)
  X_bin <- predict(dummy_fit, X)
  if (!return_matrix) {
    X_bin <- dplyr::bind_cols(
      ling_data |>
        dplyr::select(ID, CITY, STATE, tidyselect::starts_with("ZIP")),
      X_bin
    )
  }
  if (remove_zeros) {
    X_bin <- as.data.frame(X_bin) |>
      dplyr::select(-tidyselect::ends_with(".0")) |>
      as.matrix()
  }
  return(X_bin)
}


#' Collapse rare survey responses into an "other" category
#'
#' @param ling_data the linguistics data
#' @param qa_key the question-answer key
#' @param min_prop collapse responses with a proportion less than this value
#'   into an "other" category
#'
#' @returns A list of two: (1) the collapsed data frame and (2) the updated
#'   question-answer key
#' @export
collapse_survey_responses <- function(ling_data, qa_key, min_prop = 0.05) {
  qa_key <- qa_key |>
    dplyr::mutate(
      answer = stringr::str_trim(answer),
      new_answer = dplyr::case_when(
        percentage < (min_prop * 100) ~ "other",
        TRUE ~ answer
      )
    ) |>
    dplyr::arrange(-percentage)

  collapsed_ling_data <- ling_data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("Q"),
        function(x) {
          cur_qid <- as.numeric(stringr::str_remove(dplyr::cur_column(), "^Q"))
          qid_key <- qa_key |>
            dplyr::filter(qid == cur_qid)
          factor(x, levels = qid_key$answer_num, labels = qid_key$new_answer) |>
            as.numeric()
        }
      )
    )
  collapsed_qa_key <- qa_key |>
    dplyr::group_by(qid) |>
    dplyr::mutate(
      answer_num = 1:dplyr::n()
    ) |>
    dplyr::group_by(
      qid, question, new_answer
    ) |>
    dplyr::summarise(
      answer_num = min(answer_num),
      percentage = sum(percentage),
      .groups = "drop"
    ) |>
    dplyr::rename(
      answer = new_answer
    )
  return(list(ling_data = collapsed_ling_data, qa_key = collapsed_qa_key))
}


#' Aggregate survey responses by county
#'
#' @param ling_data the linguistics data
#'
#' @returns A data frame with one row per county and each column is a question.
#'   The (i, j) value in this data frame is the most popular response to
#'   question j in county i.
#' @export
aggregate_survey_response_by_county <- function(ling_data) {
  is_onehot <- all(
    as.matrix(ling_data |> dplyr::select(tidyselect::starts_with("Q"))) %in% c(0, 1)
  )
  if (!isTRUE(is_onehot)) {
    stop(
      "Input data must be one-hot encoded to aggregate by county. ",
      "Run one_hot_ling_data(ling_data, return_matrix = FALSE) first."
    )
  }
  if (!("ZIP_county" %in% names(ling_data)) ||
      !("ZIP_state" %in% names(ling_data))) {
    stop(
      "Input data must have ZIP_county and ZIP_state columns.",
      "Try running one_hot_ling_data(ling_data, return_matrix = FALSE) first."
    )
  }
  ling_data_by_county <- ling_data |>
    # aggregate response by county
    dplyr::group_by(ZIP_county, ZIP_state) |>
    dplyr::summarise(
      dplyr::across(tidyselect::starts_with("Q"), mean),
      ZIP_lat = mean(ZIP_lat),
      ZIP_long = mean(ZIP_long),
      ZIP_state_abb = dplyr::first(ZIP_state_abb),
      .groups = "drop"
    )
  return(ling_data_by_county)
}


#' Remove samples with too many missing values
#'
#' @param ling_data the linguistics data
#' @param min_answers the minimum number of answers required for a sample
#'   to be retained
#'
#' @returns A data frame with samples removed
#' @export
remove_samples <- function(ling_data, min_answers = 50) {
  X <- ling_data |>
    dplyr::select(tidyselect::starts_with("Q"))
  num_answered <- rowSums(X != 0)
  ling_data <- ling_data[num_answered >= min_answers, , drop = FALSE]
  return(ling_data)
}


#' Remove samples from Hawaii and Alaska
#'
#' @param ling_data the linguistics data
#'
#' @returns A data frame with samples from Hawaii and Alaska removed
#' @export
keep_contiguous_us <- function(ling_data) {
  ling_data <- ling_data |>
    dplyr::filter(!(ZIP_state_abb %in% c("HI", "AK")))
  return(ling_data)
}

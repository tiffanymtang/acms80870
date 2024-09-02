#' Clean NAs in columns
#'
#' @description This function cleans NAs in columns of a data frame.
#'
#' @param X A data frame.
#' @param clean_mode A character string. One of "remove" or "impute_mean".
#' @param verbose An integer. The level of verbosity.
#'
#' @return A data frame, cleaned without NAs.
#'
#' @examples
#' X <- data.frame(a = c(1, 2, NA), b = c(NA, 3, 4), c = c(1, 2, 3))
#' X_cleaned <- clean_na_cols(X, clean_mode = "remove")
#' X_cleaned <- clean_na_cols(X, clean_mode = "impute_mean")
#'
#' @export
clean_na_cols <- function(X,
                          clean_mode = c("remove", "impute_mean"),
                          verbose = 0) {

  clean_mode <- match.arg(clean_mode)

  X <- as.data.frame(X)
  col_nas <- apply(X, 2, function(x) sum(is.na(x)))

  if (verbose >= 1) {
    print(table(col_nas, dnn = "Frequency Table: #NAs per Column"))
  }
  if (verbose >= 2) {
    cat("\nColumns with NAs: \n")
    cat(names(col_nas)[col_nas > 0], sep = "\n")
  }
  if (verbose >= 1) {
    if (identical(clean_mode, "remove")) {
      cat(paste("\nRemoved", sum(col_nas > 0), "features with NAs\n"))
    } else if (identical(clean_mode, "impute_mean")) {
      cat(paste("\nImputed mean for", sum(col_nas > 0), "features with NAs\n"))
    }
  }

  if (identical(clean_mode, "remove")) {
    X_cleaned <- X[, col_nas == 0, drop = FALSE]
  } else if (identical(clean_mode, "impute_mean")) {
    X_cleaned <- X |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(names(col_nas)[col_nas > 0]),
          ~ tidyr::replace_na(.x, mean(.x, na.rm = TRUE))
        )
      )
  }

  return(X_cleaned)
}

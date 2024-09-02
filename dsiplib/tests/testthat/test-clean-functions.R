test_that("clean_na_cols works", {

  X <- data.frame(a = c(1, 2, NA), b = c(NA, 3, 4), c = c(1, 2, 3))
  X_removed <- data.frame(c = c(1, 2, 3))
  X_imputed <- data.frame(a = c(1, 2, 1.5), b = c(3.5, 3, 4), c = c(1, 2, 3))

  X_cleaned <- clean_na_cols(X, clean_mode = "remove")
  expect_equal(X_cleaned, X_removed)

  X_cleaned <- clean_na_cols(X, clean_mode = "impute_mean")
  expect_equal(X_cleaned, X_imputed)

  expect_error(clean_na_cols(X, clean_mode = "none"))
})

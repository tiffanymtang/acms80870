fit_rf_loo <- function(i, x, y, ...) {
  fit <- ranger::ranger(
    x = x[-i, , drop = FALSE],
    y = y[-i],
    ...
  )
  preds <- as.character(predict(fit, x[i, , drop = FALSE])$predictions)
  return(preds)
}

fit_rf_loo <- function(i, x, y, ...) {
  fit <- ranger::ranger(
    x = x[-i, , drop = FALSE],
    y = y[-i],
    ...
  )
  preds <- as.character(predict(fit, x[i, , drop = FALSE])$predictions)
  return(preds)
}


fit_knn_loo <- function(i, x, y, ...) {
  train_control <- caret::trainControl(
    method = "cv",
    number = 5
  )
  tune_grid <- expand.grid(k = c(5, 10, 15))
  fit <- caret::train(
    x = x[-i, , drop = FALSE],
    y = y[-i],
    method = "knn",
    ...,
    trControl = train_control,
    tuneGrid = tune_grid,
  )
  preds <- as.character(predict(fit, x[i, , drop = FALSE]))
  return(preds)
}

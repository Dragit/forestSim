#' Incrementally train a random forest and record performance
#'
#' @param data   Training data.frame with predictors and target
#' @param target Name of target column (string)
#' @param ntree  Number of trees to grow
#' @param mtry   Number of variables randomly sampled at each split
#' @param test   Optional test set (must include target column)
#' @return       tibble with tree-level performance (OOB + test) and model
#' @export
simulate_forest <- function(data, target, ntree = 100, mtry = NULL, test = NULL) {
  stopifnot(target %in% names(data))

  # Prepare training data
  y <- data[[target]]
  x <- data[setdiff(names(data), target)]

  # Set mtry default if needed
  if (is.null(mtry)) {
    mtry <- if (is.factor(y)) floor(sqrt(ncol(x))) else max(1, floor(ncol(x) / 3))
  }

  # Fit the forest with inbag info to allow staged predictions
  rf <- randomForest::randomForest(x, y, ntree = ntree, mtry = mtry,
                                   keep.forest = TRUE, keep.inbag = TRUE,
                                   do.trace = FALSE)

  # OOB performance
  perf_oob <- if (is.factor(y)) {
    1 - rf$err.rate[, "OOB"] # Accuracy
  } else {
    rf$mse[-1] # MSE
  }

  # Test performance
  perf_test <- NULL
  if (!is.null(test)) {
    stopifnot(all(names(x) %in% names(test)))
    test_y <- test[[target]]
    test_x <- test[setdiff(names(test), target)]

    # Predict on test set after each tree
    preds_list <- predict(rf, test_x, predict.all = TRUE)$individual
    perf_test <- vapply(
      1:ntree,
      function(i) {
        pred_i <- preds_list[, 1:i, drop = FALSE]
        avg_pred <- if (is.factor(y)) {
          # Majority vote
          apply(pred_i, 1, function(row) names(sort(table(row), decreasing = TRUE))[1])
        } else {
          rowMeans(pred_i)
        }
        if (is.factor(y)) {
          mean(avg_pred == test_y)
        } else {
          mean((avg_pred - test_y)^2)  # MSE
        }
      },
      numeric(1)
    )
  }

  # Assemble result
  out <- tibble::tibble(
    tree_n        = seq_len(ntree),
    performance_oob  = perf_oob,
    performance_test = if (!is.null(perf_test)) perf_test else NA
  )

  # Attach model and type
  attr(out, "rf_model") <- rf
  attr(out, "task_type") <- if (is.factor(y)) "classification" else "regression"
  out <- structure(out, class = c("forestSim", class(out)))
  return(out)
}

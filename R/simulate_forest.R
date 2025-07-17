#' Incrementally train a random forest and record performance
#'
#' @param data            Training data.frame with predictors and target
#' @param target          Name of target column (string)
#' @param ntree           Number of trees to grow
#' @param mtry            Number of variables randomly sampled at each split
#' @param test            Optional test set (must include target column)
#' @param test_fraction   Optional, float between 1 and 0, that indicated the 
#'                        fraction of the datasset to be used as a test
#' @param test_frac_seed  Seed to make data set split reproducible
#' @return       tibble with tree-level performance (OOB + test) and model
#' @export
#' @examples
#' # Train the random forest model using the simulate_forest function
#' result <- simulate_forest(
#'   data = iris,
#'   target = "Species",
#'   ntree = 100,
#'   mtry = 2,
#'   test_fraction = 0.3,
#'   test_frac_seed = 42
#' )
simulate_forest <- function(data,
                            target,
                            ntree = 100,
                            mtry = NULL,
                            test_fraction = NULL,
                            test_frac_seed = NULL,
                            test = NULL
                            ) {
  stopifnot(target %in% names(data))

# Automatically split test set if requested
if (is.null(test) && !is.null(test_fraction)) {
    stopifnot(test_fraction > 0 && test_fraction < 1)
  if (!is.null(test_frac_seed)) {
    set.seed(test_frac_seed)
  }
    n <- nrow(data)
    test_indices <- sample(n, size = floor(n * test_fraction))
    test <- data[test_indices, ]
    data <- data[-test_indices, ]
  }
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
    rf$mse # MSE
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

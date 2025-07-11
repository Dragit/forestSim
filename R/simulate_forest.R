#' Incrementally train a random forest and record performance
#'
#' @param data   data.frame with predictors and target
#' @param target name of target column (string)
#' @param ntree  total trees to grow
#' @param mtry   variables tried at each split (passed to randomForest)
#' @param test   optional test set (same columns as `data`)
#' @return       tibble with tree‑level metrics and the full model
#' @export
simulate_forest <- function(data, target, ntree = 100, mtry = NULL, test = NULL) {
  stopifnot(target %in% names(data))
  y   <- data[[target]]
  x   <- data[setdiff(names(data), target)]
  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(x)))  # default for classification
  }
  rf  <- randomForest::randomForest(x, y, ntree = ntree, mtry = mtry,
                                    keep.forest = TRUE, keep.inbag = TRUE,
                                    do.trace = FALSE)

  # Accuracy after each tree (OOB by default)
  if (is.factor(y)) {
    # Classification
    acc <- 1 - rf$err.rate[, "OOB"]   # convert error to accuracy
  } else {
    # Regression
    acc <- rf$mse                    # Mean Squared Error per tree
  }

  tibble::tibble(
    tree_n   = seq_len(ntree),
    performance = acc
  ) %>%
    structure(class = c("forestSim", "tbl_df", "tbl", "data.frame"),
              rf_model = rf)
}

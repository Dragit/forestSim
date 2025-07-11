#' Plot accuracy or MSE growth over trees
#'
#' @param sim  Output from simulate_forest()
#' @return     ggplot object
#' @export
plot_accuracy_growth <- function(sim) {
  # Determine if we're plotting accuracy (classification) or MSE (regression)
  metric_label <- if ("accuracy" %in% names(sim)) {
    "Accuracy"
  } else if ("performance" %in% names(sim)) {
    if (attr(sim, "rf_model")$type == "classification") {
      "Accuracy"
    } else {
      "MSE"
    }
  } else {
    "Metric"
  }

  metric_column <- if ("accuracy" %in% names(sim)) "accuracy" else "performance"

  ggplot2::ggplot(sim, ggplot2::aes(x = tree_n, y = .data[[metric_column]])) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(metric_label, " over Trees"),
      x = "Number of Trees",
      y = metric_label
    )
}

#' Plot performance over trees (OOB and optional test set)
#' 
#' @param sim  Output from simulate_forest()
#' @return     ggplot object
#' @export
plot_accuracy_growth <- function(sim) {
  type <- attr(sim, "task_type")
  metric_label <- if (type == "classification") "Accuracy" else "MSE"

  # Reshape for ggplot
  long <- tidyr::pivot_longer(sim,
    cols = starts_with("performance"),
    names_to = "dataset",
    values_to = "value"
  )
  long$dataset <- dplyr::recode(long$dataset,
    performance_oob = "OOB",
    performance_test = "Test"
  )

  ggplot2::ggplot(long, ggplot2::aes(x = tree_n, y = value, color = dataset)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(metric_label, " over Trees"),
      x = "Number of Trees",
      y = metric_label,
      color = "Dataset"
    )
}

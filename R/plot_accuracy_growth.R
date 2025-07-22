#' Plot performance over trees (OOB-Accuracy or MSE and optional test set)
#' 
#' Produces a line plot showing either accuracy or MSE across trees for a
#' random forest simulation. 
#' @param sim  Output from simulate_forest()
#' @return     ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate recode
#' @examples
#' # Sample 70% of rows for training
#' sim <- simulate_forest_multi(
#'  data = ChickWeight, 
#'  target = "weight", 
#'  ntree = 150,
#'  mtry = 2,
#'  test_fraction = 0.3,
#'  test_frac_seed = 42
#' )
#' # run the funtion to plot the result
#' plot_accuracy_growth(sim)
#' # plot another dataset
#' sim2 <- simulate_forest(
#'   data = head(ggplot2::diamonds, n = 1000),
#'   target = "cut",
#'   ntree = 100,
#'   mtry = 2,
#'   test_fraction = 0.5,
#'   )
#' plot_accuracy_growth(sim2)
plot_accuracy_growth <- function(sim) {
  type <- attr(sim, "task_type")
  metric_label <- if (type == "classification") "OOB-Accuracy" else "MSE"
  y_axis_label <- if (type == "classification") "OOB-Accuracy/Accuracy" else "MSE"
  long_metric_label <- if (type == "classification") "Accuracy" else "Mean Squared Error (MSE)"

  # Reshape for ggplot
  long <- tidyr::pivot_longer(sim,
    cols = starts_with("performance"),
    names_to = "dataset",
    values_to = "value"
  )

  # Remove rows with missing performance values (e.g. if no text set is given)
  long <- dplyr::filter(long, !is.na(value))

  long$dataset <- dplyr::recode(long$dataset,
    performance_oob = metric_label,
    performance_test = "Test"
  )

  ggplot2::ggplot(long, ggplot2::aes(x = tree_n, y = value, color = dataset)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(long_metric_label, " change over Trees"),
      x = "Number of Trees",
      y = y_axis_label,
      color = "Dataset"
    )
}

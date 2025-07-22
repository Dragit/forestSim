#' Plot performance growth across multiple forest runs
#'
#' Produces a line plot showing either accuracy or MSE across trees for multiple
#' random forest simulations. Each `(run, dataset)` pair is a separate line.
#' OOB-Accuracy and Test (if used) are shown using different linetypes, but 
#' share the same color per run.
#'
#' @param sim_multi Output from `simulate_forest_multi()`
#'
#' @return A ggplot object
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
#'  n_runs = 4,
#'  seeds = c(42, 43, 44, 45),
#'  test_fraction = 0.3,
#' )
#' plot_accuracy_growth_multi(sim)
#' # plot another dataset
#' sim2 <- simulate_forest_multi(
#'   data = head(ggplot2::diamonds, n = 1000),
#'   n_runs = 4,
#'   target = "cut",
#'   ntree = 100,
#'   mtry = 2,
#'   test_fraction = 0.5,
#'   )
#' plot_accuracy_growth_multi(sim2)
plot_accuracy_growth_multi <- function(sim_multi) {
  type <- attr(sim_multi, "task_type")
  metric_label <- if (type == "classification") "OOB" else "MSE"
  long_metric_label <- if (type == "classification") "Accuracy" else "Mean Squared Error (MSE)"
  y_axis_label <- if (type == "classification") "OOB-Accuracy/Accuracy" else "MSE"

  long <- tidyr::pivot_longer(
    sim_multi,
    cols = starts_with("performance"),
    names_to = "dataset",
    values_to = "value"
  ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      dataset = dplyr::recode(
        dataset,
        performance_oob = metric_label,
        performance_test = "Test"
      ),
      group = paste(run, dataset, sep = "_")
    )

  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = tree_n,
      y = value,
      color = run,
      linetype = dataset,
      group = group
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(long_metric_label, " over Trees (multiple runs)"),
      x = "Number of Trees",
      y = y_axis_label,
      color = "Runs",
      linetype = "Dataset"
    )
}

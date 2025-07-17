#' Plot performance growth across multiple forest runs
#'
#' Produces a line plot showing either accuracy or MSE across trees for multiple
#' random forest simulations. Each `(run, dataset)` pair is a separate line.
#' OOB and Test are shown using different linetypes, but share the same color 
#' per run.
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
#'  ChickenWeight, 
#'  target = "weight", 
#'  ntree = 150,
#'  mtry = 2,
#'  n_runs = 4,
#'  seeds = c(42, 43, 44, 45),
#'  test_fraction = 0.3,
#'  test_frac_seed = 42
#' )
#' plot_accuracy_growth_multi(sim)
plot_accuracy_growth_multi <- function(sim_multi) {
  type <- attr(sim_multi, "task_type")
  metric_label <- if (type == "classification") "OOB" else "MSE"
  long_metric_label <- if (type == "classification") "Accuracy" else "MSE"

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
      y = metric_label,
      color = "Runs",
      linetype = "Dataset"
    )
}

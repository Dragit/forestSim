#' Run multiple random forest simulations in parallel
#'
#' This is a wrapper around `simulate_forest()` that runs it multiple times in 
#' parallel.
#'
#' @param n_runs  Number of simulations to run (default: 4)
#' @param seeds           Optional vector of random seeds for each thread; will 
#'                        be generated if NULL, this does also influence the 
#'                        selection of testing datata when using (test_fraction)
#' @param data            Training data.frame with predictors and target
#' @param target          Name of target column (string)
#' @param ntree           Number of trees to grow
#' @param mtry            Number of variables randomly sampled at each split
#' @param test            Optional test set (must include target column), for
#'                        testing the forest with seperate testing data.
#'                        Excludes use of (test_fraction) parameter!
#' @param test_fraction   Optional, float between 1 and 0, that indicated the 
#'                        fraction of the datasset to be used as a test
#'                        Excludes use of (test) parameter!
#' @return A data frame combining all runs, with an added `run` column
#' @export
#' @importFrom furrr future_map2
#' @importFrom future plan multisession
#' @importFrom dplyr mutate bind_rows
#' @examples
#' # Train 4 random forest model using the simulate_forest function
#' result1 <- simulate_forest_multi(
#'   data = iris,
#'   target = "Species",
#'   ntree = 100,
#'   mtry = 2,
#'   test_fraction = 0.3,
#'   n_runs = 4,
#'   seeds = c(42, 43, 44, 45)
#' )
#'# Train the random forest model using the diamonds dataset
#'result2 <- simulate_forest_multi(
#'  data = head(ggplot2::diamonds, n = 1000),
#'  n_runs = 4,
#'  target = "cut",
#'  ntree = 100,
#'  test_fraction = 0.5,
#'  )
#'  plot_accuracy_growth_multi(result)
simulate_forest_multi <- function(data, 
                                  target, 
                                  ntree = 100,
                                  n_runs = 4, 
                                  seeds = NULL, 
                                  mtry = NULL,
                                  test_fraction = NULL,
                                  test = NULL
                                  ) {
  future::plan(future::multisession, workers = n_runs)

  if (is.null(seeds)) {
    seeds <- sample.int(1e6, n_runs, replace = FALSE)
  }

  results <- furrr::future_map2(
    .x = seeds,
    .y = seq_len(n_runs),
    .f = \(s, id) {
      # set.seed makes tree branching itself more reproduceable
      # it also effects how the test set is generated!
      set.seed(s) 
      sim <- simulate_forest(data = data, target = target, ntree = ntree,
                             test = test, test_frac_seed = NULL, mtry = mtry,
                             test_fraction = test_fraction)
      sim$run <- paste0("run", id)
      sim
    },
  .options = furrr::furrr_options(seed = TRUE)
  )

  dplyr::bind_rows(results)
}

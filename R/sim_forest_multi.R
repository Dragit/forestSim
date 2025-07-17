#' Run multiple random forest simulations in parallel
#'
#' This is a wrapper around `simulate_forest()` that runs it multiple times in parallel.
#'
#' @param n_runs  Number of simulations to run (default: 4)
#' @param seeds   Optional vector of random seeds for each thread; will be generated if NULL
#' @param data            Training data.frame with predictors and target
#' @param target          Name of target column (string)
#' @param ntree           Number of trees to grow
#' @param mtry            Number of variables randomly sampled at each split
#' @param test            Optional test set (must include target column)
#' @param test_fraction   Optional, float between 1 and 0, that indicated the 
#'                        fraction of the datasset to be used as a test
#' @param test_frac_seed  Seed to make data set split reproducible
#' @return A data frame combining all runs, with an added `run` column
#' @export
#' @importFrom furrr future_map2
#' @importFrom future plan multisession
#' @importFrom dplyr mutate bind_rows
#' @examples
#' # Train 4 random forest model using the simulate_forest function
#' result <- simulate_forest_multi(
#'   train = iris,
#'   target = "Species",
#'   ntree = 100,
#'   mtry = 2,
#'   test_fraction = 0.3,
#'   test_frac_seed = 42,
#'   n_runs = 4,
#'   seeds = c(42, 43, 44, 45)
#' )
simulate_forest_multi <- function(train, target, ntree = 50,
                                  test = NULL, n_runs = 4, seeds = NULL, ...) {
  future::plan(future::multisession, workers = n_runs)

  if (is.null(seeds)) {
    seeds <- sample.int(1e6, n_runs, replace = FALSE)
  }

  results <- furrr::future_map2(
    .x = seeds,
    .y = seq_len(n_runs),
    .f = \(s, id) {
      set.seed(s)
      sim <- simulate_forest(train, target = target, ntree = ntree, test = test, ...)
      sim$run <- paste0("run", id)
      sim
    },
  .options = furrr::furrr_options(seed = TRUE)
  )

  dplyr::bind_rows(results)
}

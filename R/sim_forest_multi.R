#' Run multiple random forest simulations in parallel
#'
#' This is a wrapper around `simulate_forest()` that runs it multiple times in parallel.
#'
#' @inheritParams simulate_forest
#' @param n_runs  Number of simulations to run (default: 4)
#' @param seeds   Optional vector of random seeds; will be generated if NULL
#' @return A data frame combining all runs, with an added `run` column
#' @export
#' @importFrom furrr future_map2
#' @importFrom future plan multisession
#' @importFrom dplyr mutate bind_rows
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
    }
  )

  dplyr::bind_rows(results)
}

#' Extract a tree and convert it to a data.frame
#' @param sim   simulate_forest() output
#' @param tree  integer; which tree
#' @export
tree_snapshots <- function(sim, tree = 1) {
  rf <- attr(sim, "rf_model")
  randomForest::getTree(rf, k = tree, labelVar = TRUE)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_forest.R
\name{simulate_forest}
\alias{simulate_forest}
\title{Incrementally train a random forest and record performance}
\usage{
simulate_forest(data, target, ntree = 100, mtry = NULL, test = NULL)
}
\arguments{
\item{data}{Training data.frame with predictors and target}

\item{target}{Name of target column (string)}

\item{ntree}{Number of trees to grow}

\item{mtry}{Number of variables randomly sampled at each split}

\item{test}{Optional test set (must include target column)}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{  tibble with tree-level performance (OOB + test) and model
}\if{html}{\out{</div>}}
}
\description{
Incrementally train a random forest and record performance
}

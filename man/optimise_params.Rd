% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimise.R
\name{optimise_params}
\alias{optimise_params}
\title{Optimise AFLELO Params}
\usage{
optimise_params(matches, start_eval = 2000, end_eval = 2016, n = 100,
  prop_new = 0.1, generations = 10, pred_weight = 0.8, n_cores = 1)
}
\arguments{
\item{matches}{data.frame of matches for training and evaluation}

\item{start_eval}{start season for calculating fitness}

\item{end_eval}{end season for calculating fitness}

\item{n}{population size}

\item{prop_new}{proportion of new individuals added at each generation}

\item{generations}{number of generations}

\item{pred_weight}{weight given to prediction accuracy when calculating
fitness}

\item{n_cores}{number of cores to use}
}
\value{
list of data.frames describing parameters and fitness, one for each
generation
}
\description{
Attempt to find an optimal set of AFLELO Params using a genetic algorithm
}
\examples{
data("matches")
optimise_params(matches, start_eval = 1997, end_eval = 1998, n = 5,
                generations = 1)

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{new_season}
\alias{new_season}
\title{New season}
\usage{
new_season(model)
}
\arguments{
\item{model}{aflelo_model to start new season in}
}
\value{
aflelo_model at beginning of new season
}
\description{
Start a new season in an AFLELO Model
}
\examples{
model <- aflelo_model()
aflelo:::new_season(model)
}

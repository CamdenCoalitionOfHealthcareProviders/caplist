% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/northgate.R
\name{northgate}
\alias{northgate}
\title{Northgate II Function: clean Northgate II patient list}
\usage{
northgate(x)
}
\value{
Tibble
}
\description{
Northgate II Function: clean Northgate II patient list
}
\examples{
ng(x)
y <- ng(x) \%>\% write_csv(y, "y.csv", na = "")
}

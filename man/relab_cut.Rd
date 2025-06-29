% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relab_cut.R
\name{relab_cut}
\alias{relab_cut}
\title{relab_cut}
\usage{
relab_cut(vec, suffix = NULL, right = TRUE, lang = "fr")
}
\arguments{
\item{vec}{The vector to be recoded}

\item{suffix}{The suffix to be indicated after the values}

\item{right}{TRUE if categories have been created with parameter right = TRUE in cut().}

\item{lang}{The language of new labels}
}
\value{
A vector
}
\description{
Function to recode the default labels of a factor created by cut() from base R
}
\examples{
cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
table()

cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
relab_cut(suffix = "€", right = FALSE) |>
table()

}

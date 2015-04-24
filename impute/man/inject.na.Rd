\name{inject.na}
\alias{inject.na}
\title{inject.na}
\description{Injects NA values into a data frame.}
\usage{
inject.na(data, p, skip="Class")
}
\arguments{
  \item{data}{ Data.frame containing data. }
  \item{p}{ Probability of turning each element in data into a NA. }
  \item{skip}{ Vector of column names which shouldn't get any NA values. }
}
\value{
A data frame.
}
\examples{
library(impute)

data <- data.frame(x=1:10, y=letters[1:10])
inject.na(data, 0.1)
}


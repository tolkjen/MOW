\name{impute}
\alias{impute}
\alias{impute.default}
\alias{classpredict}
\alias{classpredict.default}
\alias{classpredict.rpart}
\alias{predict.impute.model}
\alias{print.impute.model}
\title{impute}
\description{Creates imputation model.}
\usage{
impute(data, method.cont="mean", method.nom="mode", backup.cont="mean", 
       backup.nom="mode", args.nom=NULL, args.cont=NULL)
}
\arguments{
  \item{data}{ Data.frame containing data. It can contain NA (missing data). }
  \item{method.cont}{ Imputation method for numerical attributes. Possible values include "mean", "median" or a regression function. }
  \item{method.nom}{ Imputation method for nominal attributes. Possible values include "mode" or a classifier function. }
  \item{backup.cont}{Imputation method for numerical attributes of the copy of the data. Correct values are "mean" or "median".}
  \item{backup.nom}{Imputation method for numerical attributes of the the copy of the data. Correct value is "mode".}
  \item{args.cont}{A list of arguments passed to the function passed as method.cont.}
  \item{args.nom}{A list of arguments passed to the function passed as method.nom.}
}
\details{
Creates an imputation model from the given data. If regression or classification functions are used, the models are generated using a copy of the original data imputed using backup.cont and backup.nom methods.
}
\value{
An imputation model object.
}
\examples{
library(impute)

data <- data.frame(x=c(1, 2, NA, 3, NA), y=c(NA, "a", "b", "c", "d"))
model <- impute(data)
predict(model, data)
}


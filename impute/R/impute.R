impute <- function(x, ...) UseMethod("impute")

impute.default <- function(data, mcont="mean", mnom="mode", ...) {
  mean_model <- function(x) { 
    mean(x[!is.na(x)]) 
  }
  
  median_model <- function(x) {
    median(x[!is.na(x)])
  }
  
  mode_model <- function(x) {
    if (!is.factor(x)) x <- factor(x)
    A <- tabulate(x)
    as.character(levels(x)[which.max(A)])
  }
  
  if (mcont == "mean")
    continous_model = mean_model
  else if (mcont == "median")
    continous_model = median_model
  else
    stop("mcont parameter must be 'mean' or 'median'.")
  
  if (mnom != "mode")
    stop("mnom parameter must be equal to 'mode'")
  
  values <- lapply(data, function(x) {
    x <- as.vector(x)
    if (mode(x) == "numeric")
      continous_model(x)
    else
      mode_model(x)
  })
  
  model = list(details=values, 
               call=match.call(),
               mcont=mcont,
               mnom=mnom)
  class(model) <- "impute.model"
  
  model
}

print.impute.model <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  
  cat("\nImputation method (continous):\n")
  print(x$mcont)
  
  cat("\nImputation method (nominal):\n")
  print(x$mnom)
  
  cat("\nImputed values:\n")
  print(data.frame(x$details), row.names=FALSE)
}

predict.impute.model <- function(object, newdata=NULL, ...) {
  replace_na <- Vectorize(function(x, value) {
    if (is.na(x)) value else x
  })
  
  col_list <- c(sapply(names(newdata), function(colname) {
    list(replace_na(as.vector(newdata[[colname]]), object[[colname]]))
  }))
  
  data.frame(col_list, row.names=seq(nrow(newdata)))
}

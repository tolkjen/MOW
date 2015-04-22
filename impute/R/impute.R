impute <- function(x, ...) UseMethod("impute")

impute.default <- function(data, ...) {
  non_na_mean <- function(x) { 
    mean(x[!is.na(x)]) 
  }
  
  non_na_dominant <- function(x) {
    if (!is.factor(x)) x <- factor(x)
    A <- tabulate(x)
    as.character(levels(x)[which.max(A)])
  }
  
  model_func <- function(x) {
    x <- as.vector(x)
    if (mode(x) == "numeric")
      non_na_mean(x)
    else
      non_na_dominant(x)
  }
  
  model <- lapply(data, model_func)
  class(model) <- "impute.model"
  
  model
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

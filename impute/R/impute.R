inject.na <- function(data, p, skip="Class") 
{
  col_skip <- as.vector(skip)
  
  get_value <- Vectorize(function(x, colname) { 
    if (runif(1) <= p && !(colname %in% col_skip))
      NA
    else
      x
  })
  
  col_list <- c(sapply(names(data), function(colname) {
    list(get_value(as.vector(data[[colname]]), colname))
  }))
  
  data.frame(col_list, row.names=seq(nrow(data)))
}

impute <- function(x, ...) UseMethod("impute")

impute.default <- function(data, method.cont="mean", method.nom="mode", backup.cont="mean", 
                           backup.nom="mode", args.nom=NULL, args.cont=NULL, ...) {
  mean_model <- function(x, colname) { 
    mean(x[!is.na(x)]) 
  }
  
  median_model <- function(x, colname) {
    median(x[!is.na(x)])
  }
  
  mode_model <- function(x, colname) {
    if (!is.factor(x)) x <- factor(x)
    A <- tabulate(x)
    as.character(levels(x)[which.max(A)])
  }
  
  fit_wrapper <- function(colname, method, args, backup) {
    attributes = names(data)[names(data) != colname]
    formula <- as.formula(paste(colname, "~."))
    present_rows <- !is.na(data[[colname]])
    do.call(method, c(list(formula=formula, data=backup[present_rows,]), args))
  }
  
  backup_model <- NULL
  if ("function" %in% c(class(method.cont), class(method.nom))) {
    backup_model <- impute(data, method.nom=backup.nom, method.cont=backup.cont)
    backup_data <- predict(backup_model, data)
  }
  
  if (class(method.cont) == "character") {
    if (method.cont == "mean")
      continous_model = mean_model
    else if (method.cont == "median")
      continous_model = median_model
    else
      stop("method.cont parameter must be 'mean' or 'median'.")
  } else if (class(method.cont) == "function") {
    continous_model <- function(x, colname) {
      fit_wrapper(colname, method.cont, args.cont, backup_data)
    }
  }
  
  if (class(method.nom) == "character") {
    if (method.nom == "mode")
      nominal_model = mode_model
    else
      stop("method.nom parameter must be equal to 'mode'")
  } else if (class(method.nom) == "function") {
    nominal_model <- function(x, colname) {
      fit_wrapper(colname, method.nom, args.nom, backup_data)
    }
  }
  
  values <- c(sapply(names(data), function(colname) {
    column <- as.vector(data[[colname]])
    if (mode(column) == 'numeric')
      list(continous_model(column, colname))
    else
      list(nominal_model(column, colname))
  }))

  model = list(details=values, 
               call=match.call(),
               backup=backup_model$details)
  class(model) <- "impute.model"
  
  model
}

print.impute.model <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  
  cat("\nImputation model:\n\n")
  for (i in 1:length(x$details)) {
    attribute <- names(x$details)[i]
    cat(paste(c(attribute, ":\n-------------\n\n"), collapse=""))
    print(x$details[[i]])
    cat("\n")
  }
  
  cat("\nImputation backup model:\n\n")
  backup_length <- length(x$backup)
  if (backup_length > 0) {
    for (i in 1:backup_length) {
      attribute <- names(x$backup)[i]
      cat(paste(c(attribute, ":\n-------------\n\n"), collapse=""))
      print(x$backup[[i]])
      cat("\n")
    }
  } else {
    cat("None\n")
  }  
}

classpredict <- function(x, ...) UseMethod("classpredict")

classpredict.default <- function(model, data, ...) {
  predict(model, data, ...)
}

classpredict.rpart <- function(model, data, ...) {
  predict(model, data, type="class", ...)
}

predict.impute.model <- function(object, newdata=NULL, ...) {
  data <- newdata
  for (i in 1:nrow(newdata)) {
    for (j in 1:ncol(newdata)) {
      if (is.na(data[i,j])) {
        colname <- names(newdata)[j]
        model <- object$details[[colname]]
        if (class(model) %in% c("numeric", "character"))
          data[i,j] <- model
        else {
          prediction <- classpredict(model, data[i,])
          if (!is.na(prediction))
            data[i,j] <- prediction
          else 
            data[i,j] <- object$backup[[colname]]
        }
      }
    }
  }
  data
}

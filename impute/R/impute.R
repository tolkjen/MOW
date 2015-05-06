inject.na <- function(data, p, skip="Class") 
{
  # Vector of names of the columns which should not get any NA values
  col_skip <- as.vector(skip)
  
  # Vector function which produces a uniform random variable and if it's less 
  # or equal to 'p', places a NA value.
  get_value <- Vectorize(function(x, colname) { 
    if (runif(1) <= p && !(colname %in% col_skip)) NA else x
  })
  
  # Create a list of columns by concatenating lists containing a single column.
  col_list <- c(sapply(names(data), function(colname) {
    list(get_value(as.vector(data[[colname]]), colname))
  }))
  
  # Produce the output data frame from the column list.
  data.frame(col_list, row.names=seq(nrow(data)))
}

impute <- function(data, method.cont="mean", method.nom="mode", backup.cont="mean", 
                           backup.nom="mode", args.nom=NULL, args.cont=NULL) {
  # Calculates the mean value of all non-NA elements in x. Use for numeric 
  # attributes.
  mean_model <- function(x, colname) { 
    mean(x[!is.na(x)]) 
  }
  
  # Calculates the median of all non-NA elements in x. Use for numeric 
  # attributes.
  median_model <- function(x, colname) {
    median(x[!is.na(x)])
  }
  
  # Calculates the dominant value (mode) of an attribute (nominal).
  mode_model <- function(x, colname) {
    if (!is.factor(x)) x <- factor(x)
    A <- tabulate(x)
    as.character(levels(x)[which.max(A)])
  }
  
  # This method returns a classification/regression model (eg. rpart) created 
  # on the backup data. Backup data is data with missing values replaced using 
  # backup.cont and backup.nom methods.
  fit_wrapper <- function(colname, method, args, backup) {
    formula <- as.formula(paste(colname, "~."))
    present_rows <- !is.na(data[[colname]])
    do.call(method, c(list(formula=formula, data=backup[present_rows,]), args))
  }
  
  # If method.cont or method.nom is a function, create the backup data from the 
  # original data and use it with this function(s).
  backup_model <- NULL
  if ("function" %in% c(class(method.cont), class(method.nom))) {
    backup_model <- impute(data, method.nom=backup.nom, method.cont=backup.cont)
    backup_data <- predict(backup_model, data)
  }
  
  # Select a function which will create imputation model for each numerical 
  # attribute and assign it to continous_model.
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
  
  # Select a function which will create imputation model for each nominal 
  # attribute and assign it to nominal_model.
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
  
  # Create imputation models (mean/median/mode value or R object) for each 
  # attribute and store them in a list.
  values <- c(sapply(names(data), function(colname) {
    column <- as.vector(data[[colname]])
    if (mode(column) == 'numeric')
      list(continous_model(column, colname))
    else
      list(nominal_model(column, colname))
  }))

  # Create the imputation model data structure.
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

# Use classpredict instead of predict so that some needed arguments are 
# provided by default (see classpredict.rpart)
classpredict <- function(model, data, ...) UseMethod("classpredict")

classpredict.default <- function(model, data, ...) {
  predict(model, data, ...)
}

# Prediction with rpart requires additional 'class' parameter.
classpredict.rpart <- function(model, data, ...) {
  predict(model, data, type="class", ...)
}

predict.impute.model <- function(object, newdata=NULL, ...) {
  # Create a copy of the data
  data <- newdata
  for (i in 1:nrow(newdata)) {
    for (j in 1:ncol(newdata)) {
      if (is.na(data[i,j])) {
        # Get the imputation model of the attribute
        colname <- names(newdata)[j]
        model <- object$details[[colname]]
        
        # If the model is a simple value, assign it to the cell. Otherwise 
        # take the current row and predict the value.
        if (class(model) %in% c("numeric", "character"))
          data[i,j] <- model
        else {
          prediction <- classpredict(model, data[i,])
          
          # If prediction didn't work, replace the NA with the backup model 
          # (which always works because it's a scalar value, not a function).
          if (!is.na(prediction))
            data[i,j] <- prediction
          else 
            data[i,j] <- object$backup[[colname]]
        }
      }
    }
  }
  # Return the modified data
  data
}

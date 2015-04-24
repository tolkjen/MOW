# Installation
Go into the directory and do the following from the command prompt/bash:
```bash
R CMD build impute
R CMD INSTALL impute
```
MS Windows users might need additional software (RTools): http://r.meteo.uni.wroc.pl/bin/windows/Rtools/

# Example usage
```R
library(impute)
data("ozone")

data <- inject.na(ozone, 0.1)
model <- impute(data, method.cont="median")
predict(model, data)
```

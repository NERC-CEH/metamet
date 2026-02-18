# MetQC gap-fill methods

The MetQC app uses multiple gap-fill methods to impute data when the recorded data maybe incorrect or unreliable. This document will outline the methods undertaken, by the app, for each gap-fill method.

| QC code value | QC Code | Meaning |
| ----- | ----- | ----- |
| 0 | raw       | Original observation: raw data. |
| 1 | missing   | Missing value: usually only a temporary state, whereafter values are imputed by one of the methods below. |
| 2 | time      | Time: values are predicted from a GAM fitted to the non-missing data, using the local trend in time plus terms for the diurnal and seasonal cycles. The effect is similar to LOESS filtering but with the addition of the diurnal and seasonal trends, useful when multiple days are missing. Generally this is the best option for most variables. |
| 3 | regn      | Regression with covariate: values are predicted from a linear regression model fitted to the non-missing data, using some other covariate selected from the data set. Can simply replace missing values with the covariate if `fit = FALSE`.|
| 4 | nightzero | Night-time zero: night-time values are forced to be zero, generally the true for incoming solar radiation variables. |
| 5 | noneg     | No negative values: any negative values are forced to be zero, true for any quantity which has to be positive e.g. rainfall. |
| 6 | zero      | All zeros: all values are forced to be zero, mainly used for snow depth in summer. |
| 7 | era5      | Regression with ERA5 covariate: values are predicted from a linear regression model fitted to using some other covariate selected from the ERA5 data set. ERA5 is a reanalysis data set based on weather forecasting models nudged with observations, run retrospectively so gives a consistent back-casting of weather variables. Generally this is the best option for variables whee a local trend in time is not predictable e.g. rainfall. |
| 8 | flagged   | Flagged data: manually flagged data, usually only a temporary state, whereafter values are imputed by one of the methods above. |

### Original observation (raw)
This method simply reverts any previously imputed (either as part of the automated process or by a user) data back to the raw observed data. This can be used to undo any changes.

### Missing
This method simply flags data as 'missing'. No further imputation is undertaken.

### Time
Fits a smoothing model (a GAM) of the variable in question against time. New values are predicted from this model.

### Regression with covariate
Fits a model between the variable in question and another variable, which the user can pick from a drop down list. New values are predicted from this model.

### Night-time zero
Sets values to zero during the night. Values measured during the day are unaffected. Night time is calculated using the position of measurement and the sun position.

### No negative values
Sets all selected points that are negative to zero.

### All zeros
Sets all selected points to zero.

### Regression with ERA5 covariate
Fits a model between the variable in question and the same variable from the ERA5 dataset. New values are predicted from this model.

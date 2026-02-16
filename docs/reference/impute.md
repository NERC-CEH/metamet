# Impute missing values in meteorological data

Imputes missing or flagged values in one or more variables of a
`metamet` object using various methods. The function supports
regression-based imputation, time-series smoothing (GAM), substitution
from reference data (ERA5), and physical constraints. All imputed values
are flagged in the quality control (QC) table.

## Usage

``` r
impute(
  v_y = NULL,
  mm,
  method = NULL,
  qc_tokeep = 0,
  selection = TRUE,
  k = 40,
  fit = TRUE,
  n_min = 10,
  x = NULL,
  lat = 55.792,
  lon = -3.243,
  plot_graph = TRUE
)
```

## Arguments

- v_y:

  Character vector of variable names (as quoted strings) to impute. If
  `NULL` (default), all variables in the data table except site and time
  are selected for imputation.

- mm:

  A `metamet` object containing observation data (`dt`), quality control
  codes (`dt_qc`), and optional reference data (`dt_ref`).

- method:

  Character string specifying the imputation method to use. If `NULL`
  (default), the method is read from the `imputation_method` column in
  `dt_meta`. Supported methods:

  `"time"`

  :   Generalized additive model (GAM) with smoothing splines over time
      and hour of day. Suitable for variables with strong
      diurnal/seasonal patterns.

  `"regn"`

  :   Linear regression against covariate `x`. Fits a model excluding
      missing values, then predicts.

  `"era5"`

  :   Substitute ERA5 reanalysis data from `dt_ref`. If fewer than
      `n_min` observations, replaces directly without fitting.

  `"noneg"`

  :   Replace negative values with zero (physical constraint).

  `"nightzero"`

  :   Replace nighttime values with zero. Uses site coordinates (`lat`,
      `lon`) to identify day/night via
      [`openair::cutData()`](https://openair-project.github.io/openair/reference/cutData.html).

  `"zero"`

  :   Replace all missing/flagged values with zero.

- qc_tokeep:

  Integer QC code(s) indicating "good" or "raw" data to retain
  unchanged. Default `0`. Data with QC codes not in `qc_tokeep` are
  candidates for imputation.

- selection:

  Logical. If `TRUE` (default), applies selection filtering from
  metadata. If `FALSE`, imputes all values matching `qc_tokeep`
  criteria.

- k:

  Integer. Smoothing basis dimension for GAM in "time" method (default:
  40). Automatically reduced if data is sparse. Controls temporal
  smoothness.

- fit:

  Logical. If `TRUE` (default), fits regression/GAM models for
  imputation. If `FALSE`, uses direct substitution (useful with "era5"
  method and minimal data).

- n_min:

  Integer. Minimum number of non-missing observations required to fit a
  model (default: 10). If fewer observations exist, "time" and "regn"
  methods skip imputation; "era5" method switches to direct
  substitution.

- x:

  Optional. Character string naming a covariate column in the data table
  for use in "regn" method. For example, `x = "PPFD_IN"` to regress
  against photosynthetic photon flux density.

- lat:

  Numeric. Latitude of the site in degrees (default: `55.792`). Used by
  "nightzero" method to calculate sunrise/sunset times.

- lon:

  Numeric. Longitude of the site in degrees (default: `-3.243`). Used by
  "nightzero" method to calculate sunrise/sunset times.

- plot_graph:

  Logical. If `TRUE` (default), generates diagnostic plots showing
  observations, reference data (if available), and QC flags. Saves PNG
  files to the `output/` directory with naming convention
  `plot_<variable>_<method>.png`.

## Value

The input `metamet` object `mm`, invisibly returned with updated `dt`
(imputed values) and `dt_qc` (new QC codes for imputed points).

## Details

\*\*Imputation Process:\*\* The function iterates over each variable in
`v_y`. For each variable: 1. Determines the imputation method (from
parameter or metadata). 2. Identifies which rows to impute based on QC
codes and `selection` flag. 3. Applies the selected imputation method.
4. Updates the QC table to flag imputed values. 5. Optionally generates
a diagnostic plot.

\*\*Minimum Data Handling:\*\* If fewer than `n_min` non-missing
observations exist: - "time" and "regn" methods skip the variable (no
imputation). - "era5" method switches to direct substitution
(`fit = FALSE`). - Other methods ("zero", "noneg", "nightzero") are
unaffected.

\*\*Data Reference:\*\* The function requires a metadata table
(`dt_meta`) describing variables, and optionally a reference table
(`dt_ref`) for ERA5 or other reanalysis data. Ensure these are present
in the `metamet` object.

\*\*Plotting:\*\* Diagnostic plots overlay observations (colored by QC
code), reference data (black line), and imputed points. Useful for
validating imputation results and identifying issues.

## See also

[`metamet`](https://nerc-ceh.github.io/metamet/reference/metamet.md) for
object structure
[`add_era5`](https://nerc-ceh.github.io/metamet/reference/add_era5.md)
for adding ERA5 reference data
[`time_average`](https://nerc-ceh.github.io/metamet/reference/time_average.md)
for temporal aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Impute from metadata method specification
mm <- impute(
  v_y = "SW_IN",
  mm = mm,
  qc_tokeep = 0,
  plot_graph = TRUE
)

# Example 2: Impute using ERA5 data, multiple variables
mm <- impute(
  v_y = c("TA", "RH"),
  mm = mm,
  method = "era5",
  fit = FALSE,
  plot_graph = TRUE
)

# Example 3: Regression imputation with covariate
mm <- impute(
  v_y = "SW_IN",
  mm = mm,
  method = "regn",
  x = "PPFD_IN",
  fit = TRUE,
  n_min = 15
)
} # }
```

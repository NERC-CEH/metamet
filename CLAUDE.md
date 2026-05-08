# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```r
# Load/install the package during development
devtools::load_all()        # load without installing (fastest for iteration)
devtools::install()         # install locally

# Documentation (roxygen2)
devtools::document()        # regenerate NAMESPACE and man/ from @-tags

# Testing
devtools::test()                                              # run all tests
testthat::test_file("tests/testthat/test_apply_qc.R")       # run one test file

# Full package check
devtools::check()

# Launch the Shiny app (requires package to be installed or loaded)
metamet::run_shiny()
```

## Architecture

### The `metamet` object

The core data structure is a named list of class `"metamet"` with five slots:

| Slot | Type | Purpose |
|---|---|---|
| `dt` | `data.table` | Observations — one row per timestamp per site |
| `dt_meta` | `data.table` | Column-level metadata: variable name, type, units, `range_min`/`range_max`, `imputation_method` |
| `dt_site` | `data.table` | Site-level metadata (lat, lon, elevation, …) |
| `dt_qc` | `data.table` | Parallel to `dt` — numeric QC codes per cell (0 = valid, other values map to gap-fill method) |
| `dt_ref` | `data.table` | Optional reference data (ERA5 reanalysis), same structure as `dt` |

`dt_meta` drives most package behaviour: it defines which columns are "time", which are "site", acceptable ranges, and the default imputation method for each variable.

### Construction pipeline (`R/metamet.R`)

`metamet()` is an S3 generic. The `metamet.character` / `.data.frame` / `.data.table` methods read and coerce inputs, then all converge on `new_metamet()` → `restrict()` → `convert_time_char_to_posix()`. `restrict()` reconciles `dt` and `dt_meta` so only columns present in both are kept.

### QC and imputation flow

1. `apply_qc()` — range-checks all variables against `dt_meta` min/max, sets out-of-range values to `NA`, and creates a fresh `dt_qc` (1 = missing/invalid, 0 = valid).
2. `impute()` — iterates variables, selects rows where `dt_qc` code is not in `qc_tokeep`, and fills them using the chosen method: `"time"` (GAM spline), `"regn"` (linear regression), `"era5"` (substitute from `dt_ref`), `"zero"`, `"noneg"`, or `"nightzero"`. Updates `dt_qc` with the method's numeric code on imputed rows.
3. `join()` — merges a validated query object back into the full dataset using `powerjoin::power_full_join` with `coalesce_yx` (second/newer object wins conflicts).

### QC code lookup table

`df_method` is a package-level `data.frame` (loaded as package data) with columns `method`, `method_longname`, and `qc`. All code that maps between human-readable method names and numeric codes goes through this table.

### Shiny app (`inst/shinyApp/metqc_app/app.R`)

Launched via `metamet::run_shiny()`. Layout: `shinydashboard` with tabs — *Choose file* → *Choose date range* (main dashboard) → *Download*.

**Reactive data flow:**

```
shinyFiles input → uploaded()  [loads .rds, exposes time/variable names]
        ↓
dateInput + retrieve_data button → df_daterange()
        ↓
subset_by_date() → mm_qry  [<<- global, intentionally — read by ggiraph_plot()]
        ↓
per-variable tabset of girafeOutput plots
        ↓
user selects points → impute() → re-render affected plot
        ↓
submitchanges button → join(uploaded()$mm, mm_qry) → saveRDS()
```

`mm_qry` is assigned with `<<-` in the server function and read as a global by `ggiraph_plot()` and `impute()` — this is deliberate so those functions do not need to be reactive themselves.

### Time handling

`utils.R` sets `Sys.setenv(TZ = "GMT")` at load time. All timestamps are kept in UTC/GMT throughout. `dt_meta` stores the `time_char_format` string used to parse character timestamps on construction.

### Windows file access

The Shiny app uses `shinyFiles` to browse the filesystem. On Windows the server enumerates all available drive letters; on other platforms it roots at `fs::path_home()`.

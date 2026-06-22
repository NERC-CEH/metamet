##' Example `metamet` objects
##'
##' Three example `metamet` objects containing meteorological data from the
##' Auchencorth Moss (UK-AMO) site.
##'
##' @format Each object is a `metamet` list containing:
##'   \describe{
##'     \item{`dt`}{A data table of observations with columns for time, site,
##'       and various meteorological variables (temperature, wind speed, wind
##'       direction, precipitation, etc.).}
##'     \item{`dt_meta`}{A metadata table describing each column in `dt`,
##'       including variable type, units, and other attributes.}
##'     \item{`dt_site`}{A table containing site-level information such as
##'       location (latitude/longitude) and elevation.}
##'   }
##'
##' @details
##' - `mm1`: Data from 2025-08-22, logger 03, file F02
##' - `mm2`: Data from 2025-08-22, logger 04, file F01
##' - `mm3`: Data from 2026-02-03, logger 03, file F02
##'
##' These objects are useful for testing and demonstrating the `metamet`
##' package functionality, including joining, time averaging, and subsetting
##' operations.
##'
##' @source Auchencorth Moss (UK-AMO) site data
##'
##' @name mm1
##' @docType data
##' @keywords datasets
NULL

##' @rdname mm1
##' @name mm2
NULL

##' @rdname mm1
##' @name mm3
NULL

##' Convention registry for ICOS and ERA5 variable names and units
##'
##' A named list providing the authoritative mapping between variable names and
##' canonical UDUNITS2 unit strings for each supported naming convention.
##' Used by [check_dt_meta()] and [populate_convention_cols()].
##'
##' @format A named list with one element per convention. Currently:
##'   \describe{
##'     \item{`df_icos`}{Data frame with columns `name`, `long_name`, `units`,
##'       `uri`. One row per ICOS variable (e.g. `TA`, `WS`, `P`).}
##'     \item{`df_era5`}{Data frame with the same columns for ERA5 variables
##'       (e.g. `t2m`, `ws`, `tp`). Keyed by ICOS variable (`name` is the
##'       ICOS name); multiple ICOS variables may share one ERA5 proxy.}
##'   }
##'   The `uri` column is reserved for future NVS/SKOS linked-data identifiers
##'   and is currently `NA` for all rows.
##'
##' @details
##' New conventions (e.g. CF, BODC/NVS) can be added by:
##' 1. Appending columns to `data-raw/convention_lookup.csv`.
##' 2. Adding a corresponding element to the `l_conventions` list in
##'    `data-raw/make_data-sets.R`.
##' 3. Adding `name_<conv>` / `units_<conv>` columns to `dt_meta` for the new
##'    convention. [check_dt_meta()] and [change_naming_convention()] will then
##'    pick them up automatically.
##'
##' @source Derived from `data-raw/convention_lookup.csv`, which consolidates
##'   ICOS ETC variable definitions and ERA5 parameter names.
##'
##' @name l_conventions
##' @docType data
##' @keywords datasets
NULL

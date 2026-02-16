# Create a \`metamet\` object from various input types

Constructor function for creating \`metamet\` objects from files or data
tables. This is a generic function with methods for different input
types, allowing flexible object creation from CSV/Excel files, data
frames, or data tables.

## Usage

``` r
metamet(dt, ...)

# Default S3 method
metamet(dt, ...)

# S3 method for class 'character'
metamet(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
)

# S3 method for class 'data.frame'
metamet(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
)

# S3 method for class 'data.table'
metamet(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
)
```

## Arguments

- dt:

  The main data input. For \`metamet.character\`, this is a file path to
  a CSV or other format readable by \`data.table::fread()\`. For
  \`metamet.data.frame\` or \`metamet.data.table\`, this is the actual
  data object.

- ...:

  Additional arguments passed to methods.

- dt_meta:

  A \`data.table\`, \`data.frame\`, or file path to metadata describing
  the columns in \`dt\`. Excel files (.xlsx) are supported; otherwise
  CSV format is assumed. Required.

- dt_site:

  A \`data.table\`, \`data.frame\`, or file path to site metadata
  containing site-level information. Required.

- dt_qc:

  Optional quality control data; can be a \`data.table\`,
  \`data.frame\`, or file path. Defaults to \`NULL\`.

- dt_ref:

  Optional reference data (e.g., ERA5 reanalysis); can be a
  \`data.table\`, \`data.frame\`, or file path. Defaults to \`NULL\`.

- site_id:

  A character string specifying the site identifier. Required for
  \`metamet.character\` method. Used to tag all observations.

## Value

A \`metamet\` object containing the provided data and metadata, with
processed and validated structure.

## See also

`new_metamet` for the internal constructor

## Examples

``` r
if (FALSE) { # \dontrun{
# Create from files
mm <- metamet(
  dt = "data.csv",
  dt_meta = "metadata.xlsx",
  dt_site = "site_info.csv",
  site_id = "UK-AMO"
)

# Create from data tables (requires metadata and site data)
mm <- metamet(
  dt = my_data_table,
  dt_meta = my_metadata_table,
  dt_site = my_site_info_table,
  site_id = "UK-AMO"
)
} # }
```

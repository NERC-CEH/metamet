# read_obs_autodetect

Read a single observation file, auto-detecting whether it is a Campbell
TOA5 \`.dat\` file or a plain CSV. Dispatches to `import_campbell_data`
for TOA5 and
[`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html)
otherwise.

## Usage

``` r
read_obs_autodetect(path, campbell_args = list(), fread_args = list())
```

## Arguments

- path:

  Path to the file.

- campbell_args:

  Named list of extra arguments passed to `import_campbell_data`.

- fread_args:

  Named list of extra arguments passed to
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

A `data.table`.

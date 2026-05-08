if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".SD", ".N", ".I"))
}

utils::globalVariables(c(
  # data.table pronouns / symbols
  ".SD",
  ".N",
  ".I",
  ".BY",
  ".GRP",
  ".NGRP",

  # common column names used via NSE
  "date_field",
  "time_name",
  "DATECT",
  "type",
  "validator",
  "site",
  "name_dt",

  # temp symbols frequently used in :=
  "replicate_id",
  "time_char_format",
  "ws",
  "wd",
  "range_min",
  "range_max",
  "qc",
  "ref",
  "v_names",
  "v_name_dt",
  "v_names_era5",
  "name_icos",
  "new_names",
  "date_curr",
  "date_prev",
  "date_int",
  "imputation_method",
  "df_method",
  "checked",
  "method_longname",
  "mm",
  "mm_qry",
  "y",

  # reshape long/wide
  "var_name",
  "value",
  "N"
  # "..date_field", # these probably better changed in code so they don't appear
  # "..input_variable",
  # "..time_name",
  # "..v_name_dt",
  # "..v_names",
  # "..v_names_era5",
))

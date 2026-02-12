convert_sum_to_rate <- function(
  dt,
  var_to_convert = NA, # c("P_12_1_1", "LWS_4_1_2"),
  time_name = NA # "DATECT"
) {
  interval_length_s <- as.numeric(difftime(
    dt[2, get(time_name)],
    dt[1, get(time_name)],
    units = "secs"
  ))
  dt[,
    eval(var_to_convert) := lapply(.SD, FUN = function(x) {
      x / interval_length_s
    }),
    .SDcols = var_to_convert
  ]
  # the units are changed from whatever they were initially to a rate x / secs
  # add proper units handling later
  return((dt))
}

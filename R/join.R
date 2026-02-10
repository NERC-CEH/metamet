##' Join two `metamet` objects
##'
##' Combine two `metamet` objects by performing a full join on `site` and the
##' time variable. If the time variable names differ between the two objects,
##' `mm2`'s time column and corresponding metadata row are renamed to match
##' `mm1` prior to joining.
##'
##' The join is performed using `powerjoin::power_full_join()` for the main
##' data table (`dt`), the metadata table (`dt_meta`) and the site table
##' (`dt_site`). Conflict resolution uses `coalesce_yx`, which prefers values
##' from the second object (`mm2`) when both objects contain differing values
##' for the same field.
##'
##' @param mm1 A `metamet` object (a list containing at least `dt`,
##'   `dt_meta`, and `dt_site`).
##' @param mm2 A `metamet` object to be joined onto `mm1`. Values from `mm2`
##'   take precedence when conflicts occur.
##' @return A `metamet` object with merged `dt`, `dt_meta`, and `dt_site` (the
##'   returned object has the structure of `mm2` but contains the combined
##'   information from both inputs).
##' @examples
##' # mm_joined <- join(mm1, mm2)
##' @export
join <- function(mm1, mm2) {
  # check the time variable are the same, and rename if not.
  time_name_1 <- mm1$dt_meta[type == "time", name_dt]
  time_name_2 <- mm2$dt_meta[type == "time", name_dt]
  if (time_name_1 != time_name_2) {
    # rename the row in dt_meta
    mm2$dt_meta[type == "time", name_dt := time_name_1]
    # rename the col in dt
    setnames(mm2$dt, time_name_2, time_name_1)
  }
  time_name <- time_name_1

  # with conflict = coalesce_yx, we take values from the second data table in preference
  mm2$dt <- powerjoin::power_full_join(
    mm1$dt,
    mm2$dt,
    by = c("site", time_name),
    conflict = coalesce_yx
  )

  # we may want to add start_date to 'by =' list to cope with same variable at
  # different times
  mm2$dt_meta <- powerjoin::power_full_join(
    mm1$dt_meta,
    mm2$dt_meta,
    by = "name_dt",
    conflict = coalesce_yx
  )

  mm2$dt_site <- powerjoin::power_full_join(
    mm1$dt_site,
    mm2$dt_site,
    by = "site",
    conflict = coalesce_yx
  )

  return(mm2)
}

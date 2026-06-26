# deals with missing comment validation, QC propagation from averaged subset to raw set
# and saves validated metamet object
mod_qc_propagation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("qc_message"))
  )
}

# Validate missing comments
validate_missing_comments <- function(mm_qry, v_names) {
  missing <- list()
  for (v in v_names) {
    rows_var <- mm_qry$dt[
      name_icos == v &
        !is.na(qc) & qc != 0L &
        !is.na(qc_orig) & qc_orig != qc
    ]
    # debugging
    print(paste("Checking variable:", v))
    print(rows_var)
    print(names(rows_var))
    print(length(rows_var$comment))

    # No imputed rows: no missing comments
    if (nrow(rows_var) == 0) {
      missing[[v]] <- FALSE
      next
    }
    # Comment column missing
    if (!"comment" %in% names(rows_var)) {
      missing[[v]] <- TRUE
      next
    }

    # Zero-length comment vector missing
    if (length(rows_var$comment) == 0) {
      missing[[v]] <- TRUE
      next
    }

    # Safe evaluation
    mask <- rows_var$comment
    missing[[v]] <- any(is.na(mask) | mask == "")
  }
  return(missing)
}

# Helper: Propagate QC from averaged subset to raw
propagate_qc_to_raw <- function(mm_qry_raw, mm_qry_avg, interval, username) {
  dt_raw <- mm_qry_raw$dt
  dt_avg <- mm_qry_avg$dt
  changed <- dt_avg[!is.na(qc) & qc != qc_orig]
  updated_count <- 0
  if (nrow(changed) == 0) {
    return(list(mm = mm_qry_raw, updated = 0))
  }
  for (i in seq_len(nrow(changed))) {
    t_end <- changed$TIMESTAMP[i]
    t_start <- t_end - interval
    rows_to_update <- dt_raw[
      TIMESTAMP > t_start & TIMESTAMP <= t_end &
        name_icos == changed$name_icos[i]
    ]
    n_before <- 0
    if (nrow(rows_to_update) > 0) {
      # Ensure qc exists
      if (!"qc" %in% names(rows_to_update)) {
        rows_to_update[, qc := NA_integer_]
      }
      mask <- rows_to_update$qc != changed$qc[i] | is.na(rows_to_update$qc)
      if (length(mask) > 0) {
        n_before <- sum(mask)
      }
      # Apply QC + comment + validator
      dt_raw[
        TIMESTAMP > t_start & TIMESTAMP <= t_end &
          name_icos == changed$name_icos[i],
        `:=`(
          qc = changed$qc[i],
          comment = changed$comment[i],
          validator = username
        )
      ]
    }
    updated_count <- updated_count + n_before
  }
  mm_qry_raw$dt <- dt_raw
  return(list(mm = mm_qry_raw, updated = updated_count))
}

# save validated metamet + CEDA
save_validated_output <- function(mm, fname, username) {
  saveRDS(
    mm,
    file = paste0(
      fs::path_ext_remove(fname),
      "_qc_by_", username,
      "_on_", Sys.Date(), ".", fs::path_ext(fname)
    )
  )
  df_ceda <- metamet:::format_for_ceda(mm)
  saveRDS(
    df_ceda,
    file = paste0(
      fs::path_ext_remove(fname),
      "_qc_by_", username,
      "_on_", Sys.Date(), "_ceda.", fs::path_ext(fname)
    )
  )
}

# SERVER MODULE
mod_qc_propagation_server <- function(
    id,
    mm_qry,
    mm_qry_raw,
    v_names,
    time_avg,
    username,
    fname,
    save_trigger
) {

  moduleServer(id, function(input, output, session) {

    # This object will hold the final raw QC-corrected dataset
    mm_final <- NULL
    observeEvent(save_trigger(), {
      # Detect whether ANY row was modified
      dt <- mm_qry()$dt
      edited_rows <- dt[
        !is.na(qc) & !is.na(qc_orig) & qc != qc_orig
      ]
      if (nrow(edited_rows) == 0) {
        showNotification(
          "No rows were modified.",
          type = "message",
          duration = 5
        )
        return()
      }
      # Validate missing comments
      missing <- validate_missing_comments(mm_qry(), v_names)
      missing_vec <- unlist(missing)
      if (length(missing_vec) == 0) {
        missing_vec <- FALSE
      }
      if (any(missing_vec)) {
        showNotification(
          "Some variables have imputed values without comments.",
          type = "error"
        )
        return()
      }

      # propagate QC back to raw if averaging was used
      ta <- time_avg()
      if (!is.null(ta) && length(ta) == 1 && ta != "none") {
        interval <- lubridate::duration(ta)

        result <- propagate_qc_to_raw(
          mm_qry_raw(),
          mm_qry(),
          interval,
          username
        )

        mm_final <<- result$mm

        showNotification(
          paste("QC updates applied to", result$updated, "raw data rows."),
          type = "message",
          duration = 6
        )

      } else {

        # No averaging → the edited mm_qry() is already raw
        mm_final <<- mm_qry()
      }

      # Prepare object for saving
      mm_save <- data.table::copy(mm_final)
      if (all(c("row_name", "datect_num") %in% names(mm_save$dt))) {
        mm_save$dt[, c("row_name", "datect_num") := NULL]
      }

      # Save output
      save_validated_output(mm_save, fname, username)

      showNotification(
        "Your file was successfully created.",
        type = "message",
        duration = 5
      )
    })
    return(reactive(mm_final))
  })
}

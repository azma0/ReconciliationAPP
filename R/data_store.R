# Local stand-in for the ActivityInfo forms used by the original app.
#
# The original read records with activityinfo::getTable() and wrote them back
# with activityinfo::importTable(). Here each "form" is a CSV file in a
# session-specific folder, so the demo runs without credentials or real data.
# Every write is recorded in change_log.csv as an audit trail.

store_path <- function(dir, form) file.path(dir, paste0(form, ".csv"))

store_read <- function(dir, form) {
  read.csv(store_path(dir, form), colClasses = "character",
           na.strings = "", check.names = FALSE, encoding = "UTF-8")
}

store_write <- function(dir, form, df) {
  write.csv(df, store_path(dir, form), row.names = FALSE, na = "",
            fileEncoding = "UTF-8")
  invisible(df)
}

# Update existing records only (never insert), matching on id_col.
# `updates` holds id_col plus the fields to change.
store_update <- function(dir, form, updates, id_col = "record_id") {
  current <- store_read(dir, form)
  fields  <- setdiff(names(updates), id_col)

  unknown <- setdiff(updates[[id_col]], current[[id_col]])
  if (length(unknown) > 0) {
    stop("Unknown record IDs in ", form, ": ", paste(unknown, collapse = ", "))
  }

  log <- list()
  for (i in seq_len(nrow(updates))) {
    row <- match(updates[[id_col]][i], current[[id_col]])
    for (f in fields) {
      old <- current[[f]][row]
      new <- as.character(updates[[f]][i])
      if (!identical(old, new)) {
        current[[f]][row] <- new
        log[[length(log) + 1]] <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          form = form, record_id = updates[[id_col]][i],
          field = f, old_value = old, new_value = new
        )
      }
    }
  }

  store_write(dir, form, current)
  if (length(log) > 0) {
    audit <- rbind(store_read(dir, "change_log"), do.call(rbind, log))
    store_write(dir, "change_log", audit)
  }
  invisible(length(log))
}

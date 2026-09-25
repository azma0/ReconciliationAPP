# Validation of a partner's reconciliation upload.
#
# Each rule is a small function that returns the rows it rejects. All rules
# run, and every problem is returned in one table, so a partner can fix the
# whole file in one pass. (The original app stopped at the first failed rule.)

# One row per problem found
new_issues <- function(rule, rows, df, message) {
  if (length(rows) == 0) {
    return(data.frame(rule = character(), row = integer(),
                      qa_code_sn = character(), message = character()))
  }
  data.frame(rule = rule, row = rows,
             qa_code_sn = df$qa_code_sn[rows], message = message)
}

to_number <- function(x) suppressWarnings(as.numeric(x))

# Dates must be written exactly as YYYY-MM-DD
to_date <- function(x) {
  ok <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  out <- rep(as.Date(NA), length(x))
  out[ok] <- suppressWarnings(as.Date(x[ok], format = "%Y-%m-%d"))
  out
}

months_before <- function(date, n) {
  seq(date, by = paste0("-", n, " months"), length.out = 2)[2]
}

batch_from_qa <- function(qa_code_sn) sub("-.*$", "", qa_code_sn)

blank <- function(x) is.na(x) | trimws(x) == ""

# upload:  partner file (all columns character)
# batches: batch register with batch_code and status
# today:   passed in so results are reproducible in tests
validate_recon_upload <- function(upload, batches, config = recon_config(),
                                  today = Sys.Date()) {
  missing_cols <- setdiff(config$required_columns, names(upload))
  if (length(missing_cols) > 0) {
    return(data.frame(
      rule = "missing_columns", row = NA_integer_, qa_code_sn = NA_character_,
      message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    ))
  }

  amount   <- to_number(upload$amount_given)
  usd      <- to_number(upload$amount_given_usd)
  date     <- to_date(upload$dist_date)
  paid     <- !blank(upload$amount_given)          # rows reporting a payment
  status   <- batches$status[match(batch_from_qa(upload$qa_code_sn),
                                   batches$batch_code)]
  earliest <- months_before(today, config$max_age_months)

  rbind(
    new_issues("qa_code_missing", which(blank(upload$qa_code_sn)), upload,
               "QA code is empty"),
    new_issues("batch_not_found",
               which(!blank(upload$qa_code_sn) & is.na(status)), upload,
               "Batch code not found in the batch register"),
    new_issues("batch_blocked", which(status %in% config$blocked_statuses),
               upload, "Batch is canceled, suspended or referred"),
    new_issues("batch_pending", which(status %in% config$pending_statuses),
               upload, "Batch is waiting for PDM/Endline cleaning"),
    new_issues("donor_invalid",
               which(!blank(upload$donor) &
                     !upload$donor %in% config$allowed_donors), upload,
               paste("Donor must be one of:",
                     paste(config$allowed_donors, collapse = ", "))),
    new_issues("amount_not_numeric", which(paid & is.na(amount)), upload,
               "Amount given is not a number"),
    new_issues("amount_zero", which(amount %in% 0), upload,
               "Amount given is 0; leave it empty and give a reason instead"),
    new_issues("amount_not_allowed",
               which(!is.na(amount) & amount != 0 &
                     !amount %in% config$allowed_amounts), upload,
               "Amount given is not one of the approved transfer values"),
    new_issues("date_missing", which(paid & blank(upload$dist_date)), upload,
               "Distribution date is required when an amount is given"),
    new_issues("date_format",
               which(!blank(upload$dist_date) & is.na(date)), upload,
               "Date must be YYYY-MM-DD, e.g. 2026-01-20"),
    new_issues("date_future", which(!is.na(date) & date > today), upload,
               "Distribution date is in the future"),
    new_issues("date_too_old", which(!is.na(date) & date < earliest), upload,
               paste("Distribution date is more than", config$max_age_months,
                     "months ago")),
    new_issues("transaction_id_missing",
               which(paid & blank(upload$transaction_id)), upload,
               "Transaction ID is required when an amount is given"),
    new_issues("transaction_number_missing",
               which(paid & blank(upload$transaction_number)), upload,
               "Transaction number is required when an amount is given"),
    new_issues("usd_out_of_range",
               which(!is.na(usd) & (usd < config$usd_range[1] |
                                    usd > config$usd_range[2])), upload,
               paste0("USD amount must be between ", config$usd_range[1],
                      " and ", config$usd_range[2])),
    new_issues("usd_local_mismatch",
               which(paid != !blank(upload$amount_given_usd)), upload,
               "Amount given and amount given (USD) must both be filled or both empty"),
    new_issues("no_amount_no_reason",
               which(!paid & blank(upload$reason_not_distributed)), upload,
               "Give either an amount or a reason the distribution did not occur")
  )
}

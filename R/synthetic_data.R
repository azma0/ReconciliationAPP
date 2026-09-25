# Synthetic data for the demo. No real beneficiary data is used anywhere.
# Dates are generated relative to `today`, so the sample files stay valid
# whenever the demo is run.

first_names <- c("\u0645\u062D\u0645\u062F", "\u0623\u062D\u0645\u062F",
                 "\u0639\u0644\u064A", "\u0635\u0627\u0644\u062D",
                 "\u062D\u0633\u0646", "\u0641\u0627\u0637\u0645\u0629",
                 "\u0645\u0631\u064A\u0645", "\u062E\u062F\u064A\u062C\u0629",
                 "\u0625\u0628\u0631\u0627\u0647\u064A\u0645",
                 "\u0639\u0628\u062F\u0627\u0644\u0644\u0647")
family_names <- c("\u0627\u0644\u0623\u062D\u0645\u062F\u064A",
                  "\u0627\u0644\u0633\u0627\u0645\u0639\u064A",
                  "\u0627\u0644\u0639\u0628\u0633\u064A",
                  "\u0627\u0644\u0634\u0631\u0639\u0628\u064A",
                  "\u0627\u0644\u062D\u0643\u064A\u0645\u064A")

fake_name <- function(n) {
  paste(sample(first_names, n, TRUE), sample(first_names, n, TRUE),
        sample(first_names, n, TRUE), sample(family_names, n, TRUE))
}

# Creates the data store (batches, households, distributions, change_log)
# in `dir` and three sample upload files in `dir/uploads`.
generate_synthetic_data <- function(dir, today = Sys.Date(), seed = 42) {
  set.seed(seed)
  dir.create(file.path(dir, "uploads"), recursive = TRUE, showWarnings = FALSE)
  day <- function(n) format(today - n, "%Y-%m-%d")

  batches <- data.frame(
    batch_code = c("PA_0101", "PB_0102", "PC_0103", "PD_0104", "PE_0105"),
    partner    = c("Partner A", "Partner B", "Partner C", "Partner D", "Partner E"),
    status     = c("Distribution", "Distribution", "Suspended",
                   "Ready for PDM", "Distribution")
  )

  n_per_batch <- 8
  households <- do.call(rbind, lapply(batches$batch_code, function(b) {
    data.frame(
      record_id  = paste0("HH-", b, "-", sprintf("%02d", 1:n_per_batch)),
      qa_code_sn = paste0(b, "-", sprintf("%04d", 1:n_per_batch)),
      hoh_name   = fake_name(n_per_batch),
      phone      = paste0("77", sample(1000000:9999999, n_per_batch)),
      id_type    = sample(c("National ID card", "Family card"), n_per_batch, TRUE),
      id_number  = as.character(sample(10000000:99999999, n_per_batch))
    )
  }))

  distributions <- data.frame(
    record_id  = sub("^HH-", "DS-", households$record_id),
    qa_code_sn = households$qa_code_sn,
    round = "1", dist_date = NA, amount_given = NA, amount_given_usd = NA,
    donor = NA, transaction_id = NA, transaction_number = NA,
    reason_not_distributed = NA, dist_status = "Planned"
  )
  # Partner B's first two households were reconciled last week
  done <- distributions$qa_code_sn %in% c("PB_0102-0001", "PB_0102-0002")
  distributions[done, c("dist_date", "amount_given", "amount_given_usd",
                        "donor", "transaction_id", "transaction_number",
                        "dist_status")] <-
    list(day(7), "122000", "230", "DONOR-A-2023", c("TX-9001", "TX-9002"),
         c("1", "2"), "Completed")

  change_log <- data.frame(timestamp = character(), form = character(),
                           record_id = character(), field = character(),
                           old_value = character(), new_value = character())

  # Fixed names for the households used in the corrections example
  pe_rows <- which(startsWith(households$qa_code_sn, "PE_"))[1:2]
  households$hoh_name[pe_rows] <- c(
    "\u0623\u062D\u0645\u062F \u0635\u0627\u0644\u062D \u0639\u0644\u064A \u0623\u0628\u064A \u0628\u0643\u0631",
    "\u0641\u0627\u0637\u0645\u0629 \u0645\u062D\u0645\u062F \u062D\u0633\u0646 \u0627\u0644\u062D\u0643\u064A\u0645\u064A"
  )

  store_write(dir, "batches", batches)
  store_write(dir, "households", households)
  store_write(dir, "distributions", distributions)
  store_write(dir, "change_log", change_log)

  # --- Sample upload 1: clean file for Partner A --------------------------
  qa_a <- households$qa_code_sn[startsWith(households$qa_code_sn, "PA_")]
  valid <- data.frame(
    qa_code_sn = qa_a, round = "1", dist_date = day(3),
    amount_given = "122000", amount_given_usd = "230", donor = "DONOR-A-2023",
    transaction_id = paste0("TX-", 100 + seq_along(qa_a)),
    transaction_number = as.character(seq_along(qa_a)),
    reason_not_distributed = NA
  )
  valid[8, c("dist_date", "amount_given", "amount_given_usd", "donor",
             "transaction_id", "transaction_number")] <- NA
  valid$reason_not_distributed[8] <- "Household not present at distribution"
  write.csv(valid, file.path(dir, "uploads", "recon_valid.csv"),
            row.names = FALSE, na = "")

  # --- Sample upload 2: file with typical partner mistakes ---------------
  errors <- valid
  errors$dist_date[1]        <- format(today - 3, "%d/%m/%Y")   # wrong format
  errors$dist_date[2]        <- day(-5)                          # future
  errors$dist_date[3]        <- day(120)                         # too old
  errors$amount_given[4]     <- "12200"                          # typo
  errors$donor[5]            <- "Donor A"                        # wrong name
  errors$transaction_id[6]   <- NA                               # missing
  errors$amount_given_usd[7] <- "2300"                           # out of range
  extra <- errors[1, ]
  extra$qa_code_sn <- "PC_0103-0001"                             # suspended batch
  extra$dist_date  <- day(3)
  errors <- rbind(errors, extra)
  write.csv(errors, file.path(dir, "uploads", "recon_with_errors.csv"),
            row.names = FALSE, na = "")

  # --- Sample upload 3: household detail corrections ----------------------
  pe <- households[startsWith(households$qa_code_sn, "PE_"), ][1:4, ]
  changes <- pe[, c("qa_code_sn", "hoh_name", "phone", "id_type", "id_number")]
  # 1: same name, different spelling (alef without hamza, "Abu" not "Abi")
  #    -> approved after normalisation
  changes$hoh_name[1] <- "\u0627\u062D\u0645\u062F \u0635\u0627\u0644\u062D \u0639\u0644\u064A \u0627\u0628\u0648 \u0628\u0643\u0631"
  # 2: a completely different person -> rejected for review
  changes$hoh_name[2] <- paste("\u064A\u0648\u0633\u0641",
                               "\u0639\u0645\u0631",
                               "\u0639\u062B\u0645\u0627\u0646",
                               "\u0627\u0644\u0648\u0635\u0627\u0628\u064A")
  # 3: new phone number -> approved
  changes$phone[3] <- "771234567"
  # 4: ID type not in the approved list -> rejected
  changes$id_type[4] <- "Driving licence"
  write.csv(changes, file.path(dir, "uploads", "data_changes.csv"),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

  invisible(dir)
}

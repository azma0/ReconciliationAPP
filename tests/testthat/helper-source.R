# Load the app's functions for testing
for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# A fixed date so date rules give the same result on every run
test_today <- as.Date("2026-06-15")

# A valid single-row upload that individual tests can break on purpose
valid_row <- function(...) {
  row <- data.frame(
    qa_code_sn = "PA_0101-0001", round = "1", dist_date = "2026-06-10",
    amount_given = "122000", amount_given_usd = "230", donor = "DONOR-A-2023",
    transaction_id = "TX-1", transaction_number = "1",
    reason_not_distributed = NA_character_
  )
  changes <- list(...)
  for (n in names(changes)) row[[n]] <- changes[[n]]
  row
}

test_batches <- data.frame(
  batch_code = c("PA_0101", "PC_0103", "PD_0104"),
  status     = c("Distribution", "Suspended", "Ready for PDM")
)

rules_failed <- function(upload) {
  validate_recon_upload(upload, test_batches, today = test_today)$rule
}

test_that("rows are split into new, already reconciled and not found", {
  dist <- data.frame(
    record_id = c("D1", "D2"), qa_code_sn = c("PA_0101-0001", "PA_0101-0002"),
    round = "1", amount_given = c(NA, "122000"), reason_not_distributed = NA
  )
  upload <- rbind(valid_row(),
                  valid_row(qa_code_sn = "PA_0101-0002"),
                  valid_row(qa_code_sn = "PA_0101-0099"))
  res <- prepare_reconciliation(upload, dist)

  expect_equal(res$new$record_id, "D1")
  expect_equal(res$already$qa_code_sn, "PA_0101-0002")
  expect_equal(res$not_found$qa_code_sn, "PA_0101-0099")
})

test_that("applying reconciliation updates records, batch status and audit log", {
  dir <- tempfile(); dir.create(dir)
  generate_synthetic_data(dir, today = test_today)
  upload <- read.csv(file.path(dir, "uploads", "recon_valid.csv"),
                     colClasses = "character", na.strings = "")

  prep <- prepare_reconciliation(upload, store_read(dir, "distributions"))
  status <- apply_reconciliation(dir, prep$new)

  dist <- store_read(dir, "distributions")
  expect_true(all(dist$dist_status[dist$record_id %in% prep$new$record_id] == "Completed"))
  expect_equal(status$new_status, "Reconciliation")
  expect_gt(nrow(store_read(dir, "change_log")), 0)

  # Running the same file again must not overwrite anything
  again <- prepare_reconciliation(upload, store_read(dir, "distributions"))
  expect_equal(nrow(again$new), 0)
})

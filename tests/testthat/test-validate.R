test_that("a correct row passes every rule", {
  expect_length(rules_failed(valid_row()), 0)
})

test_that("a row with a reason and no payment passes", {
  row <- valid_row(dist_date = NA, amount_given = NA, amount_given_usd = NA,
                   donor = NA, transaction_id = NA, transaction_number = NA,
                   reason_not_distributed = "Household absent")
  expect_length(rules_failed(row), 0)
})

test_that("missing columns stop validation with one clear message", {
  res <- validate_recon_upload(valid_row()[, -1], test_batches, today = test_today)
  expect_equal(res$rule, "missing_columns")
  expect_match(res$message, "qa_code_sn")
})

test_that("batch status rules", {
  expect_true("batch_not_found" %in% rules_failed(valid_row(qa_code_sn = "ZZ_9999-0001")))
  expect_true("batch_blocked"   %in% rules_failed(valid_row(qa_code_sn = "PC_0103-0001")))
  expect_true("batch_pending"   %in% rules_failed(valid_row(qa_code_sn = "PD_0104-0001")))
})

test_that("date rules", {
  expect_true("date_format"  %in% rules_failed(valid_row(dist_date = "10/06/2026")))
  expect_true("date_format"  %in% rules_failed(valid_row(dist_date = "2026-02-30")))
  expect_true("date_future"  %in% rules_failed(valid_row(dist_date = "2026-06-16")))
  expect_true("date_too_old" %in% rules_failed(valid_row(dist_date = "2026-04-14")))
  expect_false("date_too_old" %in% rules_failed(valid_row(dist_date = "2026-04-15")))
  expect_true("date_missing" %in% rules_failed(valid_row(dist_date = NA)))
})

test_that("amount rules", {
  expect_true("amount_not_numeric" %in% rules_failed(valid_row(amount_given = "12O000")))
  expect_true("amount_zero"        %in% rules_failed(valid_row(amount_given = "0")))
  expect_true("amount_not_allowed" %in% rules_failed(valid_row(amount_given = "12200")))
  expect_true("usd_out_of_range"   %in% rules_failed(valid_row(amount_given_usd = "2300")))
  expect_true("usd_local_mismatch" %in% rules_failed(valid_row(amount_given_usd = NA)))
})

test_that("payment details are required when an amount is given", {
  expect_true("transaction_id_missing"     %in% rules_failed(valid_row(transaction_id = NA)))
  expect_true("transaction_number_missing" %in% rules_failed(valid_row(transaction_number = NA)))
  expect_true("donor_invalid"              %in% rules_failed(valid_row(donor = "Donor A")))
})

test_that("a row with neither payment nor reason is rejected", {
  row <- valid_row(dist_date = NA, amount_given = NA, amount_given_usd = NA,
                   transaction_id = NA, transaction_number = NA)
  expect_true("no_amount_no_reason" %in% rules_failed(row))
})

test_that("all problems are reported at once, not just the first", {
  row <- valid_row(dist_date = "2026-06-20", donor = "Donor A", transaction_id = NA)
  expect_setequal(rules_failed(row),
                  c("date_future", "donor_invalid", "transaction_id_missing"))
})

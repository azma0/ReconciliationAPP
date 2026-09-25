test_that("Arabic spelling variants are normalised to the same form", {
  expect_equal(normalize_arabic("\u0623\u062D\u0645\u062F"),
               normalize_arabic("\u0627\u062D\u0645\u062F"))           # alef forms
  expect_equal(normalize_arabic("\u0641\u0627\u0637\u0645\u0629"),
               normalize_arabic("\u0641\u0627\u0637\u0645\u0647"))     # taa marbuta
  expect_equal(normalize_arabic("\u0623\u0628\u064A \u0628\u0643\u0631"),
               normalize_arabic("\u0627\u0628\u0648 \u0628\u0643\u0631")) # Abi / Abu
  expect_equal(normalize_arabic("  \u0639\u0644\u064A   \u062D\u0633\u0646 "),
               "\u0639\u0644\u064A \u062D\u0633\u0646")                # spaces
})

test_that("similarity is 100 for variants and low for different names", {
  expect_equal(name_similarity("\u0623\u062D\u0645\u062F \u0639\u0644\u064A",
                               "\u0627\u062D\u0645\u062F \u0639\u0644\u064A"), 100)
  expect_lt(name_similarity("\u0641\u0627\u0637\u0645\u0629 \u0645\u062D\u0645\u062F",
                            "\u064A\u0648\u0633\u0641 \u0639\u0645\u0631"), 75)
})

test_that("changes are approved or rejected with a reason", {
  dir <- tempfile(); dir.create(dir)
  generate_synthetic_data(dir, today = test_today)
  upload <- read.csv(file.path(dir, "uploads", "data_changes.csv"),
                     colClasses = "character", na.strings = "", encoding = "UTF-8")
  res <- compare_changes(upload, store_read(dir, "households"))

  expect_equal(nrow(res), 4)
  expect_equal(res$approved, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(res$reason[4], "Unknown ID type")

  apply_changes(dir, res)
  hh <- store_read(dir, "households")
  expect_equal(hh$phone[hh$qa_code_sn == upload$qa_code_sn[3]], "771234567")
  # A rejected name change must not be written
  expect_equal(hh$hoh_name[hh$qa_code_sn == upload$qa_code_sn[2]], res$old_value[2])
})

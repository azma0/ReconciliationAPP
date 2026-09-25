# Distribution Reconciliation Validator (demo)

A Shiny app that checks partner organisations' cash-distribution results before they are written to a shared database, and handles partner corrections to household details.

This is a rebuilt, public version of a tool I developed as Information Management Officer for a five-organisation humanitarian cash consortium. The original connected to the ActivityInfo cloud platform through its API and was used by partner organisations to submit their reconciliation data. This version keeps the same logic but uses **synthetic data only** and a local CSV store in place of the API, so it runs without credentials.

## The problem

After each cash distribution, every partner reported which households were paid, how much, when, and the transaction references. Errors were common: wrong date formats, typos in amounts, missing transaction IDs, results for suspended batches, or files submitted twice. Once these reached the shared database they flowed into donor reports and were hard to trace and correct.

The app moves those checks to the point of upload. Partners get a list of every problem in their file, and only clean records that have not already been reconciled are written.

## What it does

**1. Reconciliation**

- Validates the upload against 17 rules (below) and reports **all** problems at once, with the row and QA code for each.
- Matches each row to its distribution record by QA code and round.
- Writes only records that have not been reconciled yet. Re-submitting the same file changes nothing.
- Marks records as completed, moves the batch status to "Reconciliation", and records every field change in an audit log.

**2. Household corrections**

- Compares requested changes (name, phone, ID type, ID number) with the current record.
- Normalises Arabic names before comparing them, then approves a name change only when similarity is at least 75%. A very different name suggests a different person and is sent for review instead.
- Rejects ID types that are not in the approved list.
- Writes approved changes only, one field at a time, with an audit trail.

## Validation rules

| Area         | Rules                                                                                                                  |
| ------------ | ---------------------------------------------------------------------------------------------------------------------- |
| Structure    | Required columns present; QA code not empty                                                                            |
| Batch        | Batch exists; not canceled, suspended or referred; not waiting for PDM cleaning                                        |
| Amount       | Numeric; not zero; one of the approved transfer values; USD amount within range; local and USD amounts filled together |
| Date         | Present when an amount is given; `YYYY-MM-DD`; not in the future; not older than two months                            |
| Payment      | Transaction ID and number present when an amount is given; donor name from the approved list                           |
| Completeness | Every row has either a payment or a reason it did not occur                                                            |

Rule values (donors, amounts, USD range, thresholds) live in `R/config.R`, so they can change without touching the logic.

## Arabic name matching

Names were registered by different organisations and spelled inconsistently. Before comparison, `normalize_arabic()`:

- removes diacritics and tatweel
- unifies alef forms (أ إ آ → ا), taa marbuta (ة → ه) and alef maqsura (ى → ي)
- treats forms of "Abu" (أبو / أبي / أبا) as the same word
- collapses extra spaces

`name_similarity()` then calculates Levenshtein similarity as a percentage of the longer name.

## Project structure

```
app.R                 Shiny UI and server (thin layer over the functions below)
R/config.R            Business rules and thresholds
R/validate.R          One small function per rule; returns a table of issues
R/reconcile.R         Matching uploads to records and writing results
R/name_changes.R      Arabic normalisation, similarity, change approval
R/data_store.R        CSV stand-in for the ActivityInfo API, with audit log
R/synthetic_data.R    Demo database and sample upload files
tests/testthat/       Unit tests for rules, reconciliation and name matching
```

## Running it

Requires R (4.2 or later) in a UTF-8 locale, and the packages `shiny`, `DT` and `testthat`.

```r
install.packages(c("shiny", "DT", "testthat"))
shiny::runApp()
```

In the app, download a sample file from the sidebar and upload it. The "Database" tab shows the effect of each write, including the change log.

## Tests

```r
testthat::test_dir("tests/testthat")   # or: Rscript run_tests.R
```

41 tests cover every validation rule, including date boundaries, that re-submitting a file changes nothing, and that rejected name changes are never written.

## Differences from the original

| Original                                                                                                                 | This version                                                      |
| ------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------- |
| Read and wrote records through the ActivityInfo API (`activityinfo` package), with users signing in with their API token | Local CSV store with the same read/update pattern; no credentials |
| Stopped at the first failed rule                                                                                         | Runs all rules and returns every problem                          |
| One large script with shared global state                                                                                | Separate, testable functions; state kept per session              |
| No automated tests                                                                                                       | Unit tests for rules and write-back                               |
| Wrote personal data to a separate restricted form                                                                        | Not reproduced; all data here is synthetic                        |

## Notes

All names, phone numbers, IDs, amounts and organisations in this repository are fictional.

## Author

Asmahan Al-Shameri

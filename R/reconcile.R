# Matching a validated upload to existing distribution records and
# writing the results back.

recon_fields <- c("dist_date", "amount_given", "amount_given_usd", "donor",
                  "transaction_id", "transaction_number", "reason_not_distributed")

# Split upload rows into:
#   new       - matched to a record that has not been reconciled yet
#   already   - matched to a record that already has a result (not overwritten)
#   not_found - no distribution record for this QA code and round
prepare_reconciliation <- function(upload, distributions) {
  key_up   <- paste(upload$qa_code_sn, upload$round, sep = "_")
  key_dist <- paste(distributions$qa_code_sn, distributions$round, sep = "_")
  pos <- match(key_up, key_dist)

  matched <- upload[!is.na(pos), , drop = FALSE]
  matched$record_id <- distributions$record_id[pos[!is.na(pos)]]
  existing <- distributions[pos[!is.na(pos)], , drop = FALSE]
  done <- !blank(existing$amount_given) | !blank(existing$reason_not_distributed)

  list(
    new       = matched[!done, c("record_id", recon_fields), drop = FALSE],
    already   = matched[done, c("qa_code_sn", "round"), drop = FALSE],
    not_found = upload[is.na(pos), c("qa_code_sn", "round"), drop = FALSE]
  )
}

# Write new results, mark records completed, and move affected batches
# to "Reconciliation". Returns the batch status changes for display.
apply_reconciliation <- function(dir, new_records) {
  if (nrow(new_records) == 0) return(data.frame())

  updates <- new_records
  updates$dist_status <- "Completed"
  # Rows without a payment carry no donor
  updates$donor[blank(updates$amount_given)] <- "None"
  store_update(dir, "distributions", updates)

  dist    <- store_read(dir, "distributions")
  qa      <- dist$qa_code_sn[match(new_records$record_id, dist$record_id)]
  batches <- store_read(dir, "batches")
  touched <- batches[batches$batch_code %in% unique(batch_from_qa(qa)), ]

  store_update(dir, "batches",
               data.frame(batch_code = touched$batch_code,
                          status = "Reconciliation"),
               id_col = "batch_code")

  data.frame(batch_code = touched$batch_code, old_status = touched$status,
             new_status = "Reconciliation")
}

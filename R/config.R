# Business rules for the reconciliation checks.
# Kept in one place so rules can be changed without touching the logic.
# Values here are illustrative, not the original programme's settings.

recon_config <- function() {
  list(
    # Columns a partner's upload must contain
    required_columns = c(
      "qa_code_sn", "round", "dist_date", "amount_given", "amount_given_usd",
      "donor", "transaction_id", "transaction_number", "reason_not_distributed"
    ),
    allowed_donors   = c("DONOR-A-2023", "DONOR-B-2021", "None"),
    # Cash transfers were fixed amounts in local currency
    allowed_amounts  = c(122000, 240000),
    # USD equivalent must fall in this range (catches exchange-rate typos)
    usd_range        = c(100, 350),
    # Late entries are rejected so reports for closed periods don't change
    max_age_months   = 2,
    # Batches in these statuses must not receive reconciliation data
    blocked_statuses = c("Canceled", "Suspended",
                         "Referred outside consortium", "Referred to another partner"),
    # Batches waiting on monitoring data cleaning
    pending_statuses = c("Ready for PDM"),
    # Household name edits below this similarity (%) need manual review
    name_similarity_min = 75,
    editable_fields  = c("hoh_name", "phone", "id_type", "id_number"),
    id_types = c("National ID card", "Family card", "Passport",
                 "Election document", "No ID", "Other ID", "Partner self ID")
  )
}

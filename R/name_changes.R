# Partner-requested corrections to household details.
#
# Arabic names are spelled inconsistently between registrations, so names are
# normalised before they are compared. A name change is approved only when the
# new spelling is close to the old one; a very different name suggests a
# different person and needs manual review.

normalize_arabic <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("[\u064B-\u0652\u0640]", "", x)   # diacritics and tatweel
  x <- gsub("[\u0623\u0625\u0622]", "\u0627", x) # alef forms -> bare alef
  x <- gsub("\u0629", "\u0647", x)             # taa marbuta -> haa
  x <- gsub("\u0649", "\u064A", x)             # alef maqsura -> yaa
  # Treat the forms of "Abu" (abu / abi / aba) as the same word
  x <- gsub("(^|\\s)\u0627\u0628(\u064A|\u0627)(?=\\s|$)",
            "\\1\u0627\u0628\u0648", x, perl = TRUE)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

# Levenshtein similarity as a percentage of the longer string
name_similarity <- function(old, new) {
  a <- normalize_arabic(old)
  b <- normalize_arabic(new)
  d <- mapply(function(p, q) adist(p, q)[1, 1], a, b, USE.NAMES = FALSE)
  round(100 * (1 - d / pmax(nchar(a), nchar(b), 1)), 1)
}

# Compare uploaded corrections with current household records.
# Returns one row per changed field, with a decision and the reason.
compare_changes <- function(upload, households, config = recon_config()) {
  fields <- config$editable_fields
  upload <- upload[!duplicated(upload$qa_code_sn), , drop = FALSE]
  current <- households[match(upload$qa_code_sn, households$qa_code_sn), ]

  out <- list()
  for (f in fields) {
    changed <- !is.na(current$record_id) & !blank(upload[[f]]) &
               (is.na(current[[f]]) | upload[[f]] != current[[f]])
    changed[is.na(changed)] <- FALSE
    if (!any(changed)) next
    out[[f]] <- data.frame(
      qa_code_sn = upload$qa_code_sn[changed],
      record_id  = current$record_id[changed],
      field      = f,
      old_value  = current[[f]][changed],
      new_value  = upload[[f]][changed]
    )
  }

  not_found <- upload$qa_code_sn[is.na(current$record_id)]
  changes <- if (length(out)) do.call(rbind, out) else
    data.frame(qa_code_sn = character(), record_id = character(),
               field = character(), old_value = character(),
               new_value = character())
  rownames(changes) <- NULL

  changes$similarity <- NA_real_
  is_name <- changes$field == "hoh_name"
  changes$similarity[is_name] <- name_similarity(changes$old_value[is_name],
                                                 changes$new_value[is_name])

  changes$approved <- TRUE
  changes$reason   <- "OK"

  low <- is_name & changes$similarity < config$name_similarity_min
  changes$approved[low] <- FALSE
  changes$reason[low]   <- paste0("Name similarity below ",
                                  config$name_similarity_min, "%")

  bad_id <- changes$field == "id_type" &
            !tolower(changes$new_value) %in% tolower(config$id_types)
  changes$approved[bad_id] <- FALSE
  changes$reason[bad_id]   <- "Unknown ID type"

  attr(changes, "not_found") <- not_found
  changes
}

# Write approved changes, one field at a time
apply_changes <- function(dir, changes) {
  approved <- changes[changes$approved, , drop = FALSE]
  for (f in unique(approved$field)) {
    rows <- approved[approved$field == f, ]
    upd  <- data.frame(record_id = rows$record_id)
    upd[[f]] <- rows$new_value
    store_update(dir, "households", upd)
  }
  invisible(nrow(approved))
}

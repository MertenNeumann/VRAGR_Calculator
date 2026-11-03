# Navigation status for a single case. Item scoreability comes from the scoring
# core, so partial Item 3 and PCL-R information use the same missing-data rules.
vragr_item_status <- function(data, incomplete = character()) {
  if (!is.data.frame(data) || nrow(data) != 1L) {
    stop("Provide one case for the item status display.", call. = FALSE)
  }
  issues <- validate_vragr(data)
  needs_check <- unique(c(issues$field, incomplete))
  clean <- data
  for (field in intersect(needs_check, vragr_fields)) clean[[field]] <- NA
  scores <- vragr(clean)
  result <- lapply(seq_len(12L), function(item) {
    fields <- vragr_fields[grepl(paste0("^Item_", item, "(_|$)"), vragr_fields)]
    missing <- sum(is.na(data[1, fields, drop = FALSE]))
    invalid <- any(fields %in% needs_check)
    scoreable <- !invalid && !is.na(scores[[paste0("Item_", item, "_score")]])
    state <- if (invalid) "invalid" else if (!missing) "complete" else if (scoreable) "partial" else "missing"
    data.frame(item = item, total = length(fields), missing = missing,
      scoreable = scoreable, state = state, stringsAsFactors = FALSE)
  })
  do.call(rbind, result)
}

vragr_item_status_label <- function(status, language = "en") {
  tx <- function(text) translate(text, language)
  if (status$state == "complete") return(tx("Complete"))
  count <- sprintf(tx("%d of %d missing"), status$missing, status$total)
  if (status$state == "invalid") {
    return(if (status$missing) paste(tx("Check inputs"), count, sep = " · ") else tx("Check inputs"))
  }
  if (status$total == 1L) return(tx("No information"))
  paste(count, tx(if (status$scoreable) "Scoreable" else "Item missing"), sep = " · ")
}

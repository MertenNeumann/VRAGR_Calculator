# English is the source language. German terminology follows the German manual.
# Translation is session-local and never changes model codes or uploaded values.
# Shiny may automatically source R/ files before app.R has set up its globals.
.translation_catalog <- unlist(jsonlite::fromJSON(
  file.path("R", "translations.json")), use.names = TRUE)
.vragr_schema <- jsonlite::fromJSON(file.path("R", "schema.json"), simplifyVector = FALSE)
for (group in .vragr_schema) {
  .translation_catalog[group$en] <- group$de
  .translation_catalog[group$help_en] <- group$help_de
  .translation_catalog[paste0("Item ",group$item," · ",group$en)] <- paste0("Item ",group$item," · ",group$de)
  for (field in group$fields) .translation_catalog[field$en] <- field$de
}
field_labels <- function(language="en") {
  unlist(lapply(.vragr_schema, function(g) setNames(vapply(g$fields,function(f) f[[language]], character(1)),vapply(g$fields,`[[`,character(1),"name"))))
}
choice_labels <- function(field) {
  if(field == "Item_4") c("Never married"="No","Ever married or cohabited for at least six months"="Yes")
  else if(field %in% vragr_binary) c("No"="No","Yes"="Yes")
  else if(field == "Item_2") c("No problems"="None","Slight or moderate problems"="Moderate","Severe problems"="Severe")
  else if(field == "Item_11") c("No known hands-on sexual offences"="None","Only against children under 14"="Only14","Other hands-on sexual offences"="Other","At least one female victim aged 14 or older"="Female14")
  else c("0"="0","1"="1","2"="2")
}

translate <- function(text, language = "en") {
  if (!identical(language, "de")) return(text)
  translated <- unname(.translation_catalog[match(text, names(.translation_catalog))])
  translated[is.na(translated)] <- text[is.na(translated)]
  translated
}

# Add translation markers only to application-authored text. Raw preview tables,
# filenames, observed categories, IDs and column names are never traversed.
localize_ui <- function(node, language = "en") {
  if (inherits(node, "html")) return(node)
  if (is.character(node)) {
    if (length(node) == 1L && !is.na(node) && node %in% names(.translation_catalog)) {
      return(htmltools::span(`data-i18n` = node, translate(node, language)))
    }
    return(node)
  }
  if (inherits(node, "shiny.tag")) {
    if (!is.null(node$attribs[["data-i18n-raw"]]) || !is.null(node$attribs[["data-i18n"]])) return(node)
    # Title text cannot contain child elements; i18n.js sets document.title.
    if (node$name %in% c("title", "script", "style", "option", "textarea", "code", "pre")) return(node)
    for (attribute in c("placeholder", "title", "aria-label")) {
      text <- node$attribs[[attribute]]
      if (is.character(text) && length(text) == 1L && text %in% names(.translation_catalog)) {
        node$attribs[[paste0("data-i18n-", attribute)]] <- text
        node$attribs[[attribute]] <- translate(text, language)
      }
    }
    node$children <- lapply(node$children, localize_ui, language = language)
    return(node)
  }
  if (is.list(node)) node[] <- lapply(node, localize_ui, language = language)
  node
}

# Select option markup is generated as HTML by Shiny. Attach the source labels
# explicitly instead of parsing that HTML or touching arbitrary user values.
.attach_choice_catalog <- function(control, input_id, choices) {
  labels <- names(choices)
  if (is.null(labels)) labels <- as.character(choices)
  labels[is.na(labels) | labels == ""] <- as.character(choices)[is.na(labels) | labels == ""]
  authored <- !grepl("^column_", input_id) && !input_id %in% c("batch_id", "language")
  indexes <- which(labels %in% names(.translation_catalog) & (authored | as.character(choices) == ""))
  if (!length(indexes)) return(control)
  options <- lapply(indexes, function(i) list(value = as.character(choices[[i]]), text = labels[[i]]))
  htmltools::tagQuery(control)$find("select")$addAttrs(
    `data-i18n-options` = jsonlite::toJSON(options, auto_unbox = TRUE))$allTags()
}
i18n_select_input <- function(inputId, label, choices, ...) {
  .attach_choice_catalog(shiny::selectInput(inputId, label, choices, ...), inputId, choices)
}
i18n_selectize_input <- function(inputId, label, choices, ...) {
  .attach_choice_catalog(shiny::selectizeInput(inputId, label, choices, ...), inputId, choices)
}

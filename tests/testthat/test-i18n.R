test_that("authored strings and later column controls are localised without touching raw labels", {
  withr::local_dir(root);env <- new.env(parent=globalenv());source("app.R",local=env)
  expect_equal(env$translate("Single case","de"),"Einzelfall")
  expect_equal(env$translate("Risk category","de"),"Risikokategorie")
  expect_equal(env$translate(c("No","Yes"),"de"),c("Nein","Ja"))
  expect_equal(env$translate("VRAG-R · Calculator","de"),"VRAG-R · Rechner")
  expect_equal(env$translate("No information","de"),"Keine Angaben")
  expect_equal(env$translate("Item score","de"),"Itemwert")
  expect_equal(env$translate("Normal distribution (percentile view)","de"),"Normalverteilung (Prozentrangdarstellung)")
  for(group in env$.vragr_schema) {
    expect_equal(env$translate(group$en,"de"),group$de)
    expect_equal(env$translate(group$help_en,"de"),group$help_de)
    for(field in group$fields) expect_equal(env$translate(field$en,"de"),field$de)
  }
  control <- env$i18n_select_input("column_Item_12_5","Please select",c("Please select"="","Male"="Male","Sex"="Sex"))
  html <- as.character(env$localize_ui(control,"de"))
  expect_match(html,'<option value="Male">Male</option>',fixed=TRUE)
  expect_match(html,'<option value="Sex">Sex</option>',fixed=TRUE)
  select <- htmltools::tagQuery(control)$find("select")$selectedTags()[[1]]
  catalog <- jsonlite::fromJSON(select$attribs[["data-i18n-options"]])
  expect_identical(catalog$value,"")
  raw <- htmltools::span("Single case",`data-i18n-raw`="true")
  expect_match(as.character(env$localize_ui(raw,"de")),"Single case",fixed=TRUE)
  html <- htmltools::renderTags(env$localize_ui(env$ui,"de"))$html
  expect_match(html,"OpenAI Codex",fixed=TRUE)
  expect_false(grepl("Die KI-Unterstützung ersetzt",html,fixed=TRUE))
  expect_false(grepl("AI assistance does not replace",html,fixed=TRUE))
  expect_false(grepl('class="brand-symbol"',html,fixed=TRUE))
  # Preserve the governing German Table 4 wording despite the broader English
  # category. The discrepancy is documented, not silently imported into the UI.
  expect_match(env$field_labels("de")[["Item_8_6"]],"Fahrlässige Körperverletzung",fixed=TRUE)
  expect_false(grepl("Angriff mit Verletzungsfolgen",env$field_labels("de")[["Item_8_6"]],fixed=TRUE))
  expect_equal(env$field_labels("de")[["Item_5_22"]],"Widerstand gegen Vollstreckungsbeamte")
  expect_match(env$field_labels("de")[["Item_8_15"]],"unter 16 Jahren",fixed=TRUE)
  expect_match(env$field_labels("de")[["Item_10_11"]],"nicht mitgezählt",fixed=TRUE)
  expect_match(html,"deutsche Manual maßgeblich",fixed=TRUE)
})

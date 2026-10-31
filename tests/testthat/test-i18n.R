test_that("authored strings and later column controls are localised without touching raw labels", {
  withr::local_dir(root);env <- new.env(parent=globalenv());source("app.R",local=env)
  expect_equal(env$translate("Single case","de"),"Einzelfall")
  expect_equal(env$translate("Risk category","de"),"Risikokategorie")
  expect_equal(env$translate(c("No","Yes"),"de"),c("Nein","Ja"))
  expect_equal(env$translate("VRAG-R · Calculator","de"),"VRAG-R · Rechner")
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
})

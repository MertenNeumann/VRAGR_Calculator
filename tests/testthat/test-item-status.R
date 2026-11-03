source(file.path(root, "R/item-status.R"))

test_that("item status counts missing inputs independently of whole-case scoreability", {
  empty <- minimum_case()
  empty[] <- lapply(empty, function(value) NA)
  status <- vragr_item_status(empty)
  expect_identical(status$total, c(1L, 1L, 6L, 1L, 27L, 1L, 1L, 18L, 1L, 12L, 1L, 5L))
  expect_identical(status$missing, status$total)
  expect_true(all(status$state == "missing"))
  expect_false(any(status$scoreable))
  empty$Item_1 <- "Yes"
  expect_identical(vragr_item_status(empty)$state[1], "complete")
  expect_identical(vragr(empty)$Status, "not_calculable")
})

test_that("partially known items follow the scoring core's distinct missing-data rules", {
  data <- minimum_case()
  data[paste0("Item_3_", 2:6)] <- NA
  data$Item_5_1 <- NA
  data$Item_8_1 <- NA
  data$Item_10_1 <- NA
  data$Item_12_5 <- NA
  original <- data
  status <- vragr_item_status(data)
  expect_identical(status$state[c(3, 5, 8, 10, 12)], c("partial", "missing", "missing", "missing", "partial"))
  expect_identical(status$missing[c(3, 5, 8, 10, 12)], c(5L, 1L, 1L, 1L, 1L))
  expect_identical(status$scoreable, !is.na(unlist(vragr(data)[paste0("Item_",1:12,"_score")], use.names = FALSE)))
  expect_identical(data, original)
  data$Item_12_4 <- NA
  expect_identical(vragr_item_status(data)$state[12], "missing")
})

test_that("invalid or unconfirmed inputs cannot appear complete or poison other item statuses", {
  data <- minimum_case()
  data$Item_1 <- "unknown"
  data$Item_5_1 <- -1
  data$Item_7 <- NA
  status <- vragr_item_status(data, incomplete = "Item_7")
  expect_identical(status$state[c(1,5,7)], rep("invalid",3))
  expect_false(any(status$scoreable[c(1,5,7)]))
  expect_true(all(status$state[-c(1,5,7)] == "complete"))
})

test_that("live item indicators respect explicit information switches and preserve inputs", {
  withr::local_dir(root)
  app_env <- new.env(parent = globalenv())
  source("app.R", local = app_env)
  shiny::testServer(app_env$server, {
    expect_true(all(single_status()$state == "missing"))
    session$setInputs(single_info_5 = "known")
    expect_identical(single_status()$state[5], "complete")
    # Empty numeric browser controls can arrive as logical NA, not NA_real_.
    for (blank in list(NA, NA_real_)) {
      session$setInputs(single_Item_5_1 = blank)
      expect_identical(single_status()$state[5], "invalid")
      expect_true("Item_5_1" %in% single_state()$incomplete)
    }
    session$setInputs(single_Item_5_1 = -1)
    expect_identical(single_status()$state[5], "invalid")
    session$setInputs(single_info_5 = "missing")
    expect_identical(single_status()$state[5], "missing")
    expect_equal(single_status()$missing[5], 27)
    session$setInputs(single_info_5 = "known")
    expect_identical(single_status()$state[5], "invalid")
    session$setInputs(single_Item_3_1 = "No", single_Item_12_1 = "0",
      single_Item_12_2 = "0", single_Item_12_3 = "0", single_Item_12_4 = "0")
    expect_identical(single_status()$state[c(3,12)], c("partial","partial"))
    expect_match(output$single_item_status_3$html, "5 of 6 missing")
    snapshot <- single_state()
    session$setInputs(single_items = c("item3", "item5"), language = "de")
    expect_identical(single_state(), snapshot)
    expect_identical(input$single_items, c("item3","item5"))
    expect_match(output$single_item_status_3$html, "5 von 6 fehlen")
    session$setInputs(single_info_7 = "known")
    expect_identical(single_status()$state[7], "invalid")
    session$setInputs(single_Item_7 = 28)
    expect_identical(single_status()$state[7], "complete")
  })
})

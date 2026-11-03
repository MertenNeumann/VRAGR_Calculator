test_that("simulated complete inputs can be scored with the standalone core", {
  withr::local_seed(130)
  data <- sim_vrag_data(500, prob_missing = 0)
  expect_identical(names(data), vragr_fields)
  expect_identical(nrow(data), 500L)
  expect_false(anyNA(data))
  scores <- vragr(data)
  expect_true(all(scores$Status == "calculated"))
  expect_true(all(scores$Score >= -34 & scores$Score <= 46))
})

test_that("simulation follows caller seeds and supports empty and missing data", {
  withr::local_seed(131)
  before <- .Random.seed
  first <- sim_vrag_data(20, prob_missing = 0.3)
  expect_false(identical(.Random.seed, before))
  set.seed(131)
  expect_identical(sim_vrag_data(20, prob_missing = 0.3), first)
  expect_true(anyNA(first))
  expect_false(all(is.na(first)))
  expect_true(all(is.na(sim_vrag_data(3, prob_missing = 1))))

  before <- .Random.seed
  empty <- sim_vrag_data(0)
  expect_identical(names(empty), vragr_fields)
  expect_identical(nrow(empty), 0L)
  expect_identical(nrow(vragr(empty)), 0L)
  expect_identical(.Random.seed, before)
})

test_that("invalid simulation arguments fail before drawing random values", {
  withr::local_seed(132)
  before <- .Random.seed
  invalid_n <- list(NULL, numeric(), c(1, 2), NA_real_, NaN, Inf, -1, 0.5,
    .Machine$integer.max + 1, TRUE, "1", 1 + 0i, matrix(1), factor("1"),
    as.Date("2026-01-01"))
  for (value in invalid_n) expect_error(sim_vrag_data(value), "N must be")
  invalid_prob <- list(NULL, numeric(), c(0, 1), NA_real_, NaN, Inf, -0.1,
    1.1, FALSE, "0", 0 + 0i, matrix(0), factor("0"))
  for (value in invalid_prob) {
    expect_error(sim_vrag_data(1, prob_missing = value), "prob_missing must be")
  }
  expect_error(sim_vrag_case(envir = list()), "envir must be")
  expect_error(sim_vrag_case(prob_missing = Inf, envir = NULL), "prob_missing must be")
  expect_identical(.Random.seed, before)
})

test_that("single-case simulation can avoid or explicitly target item bindings", {
  withr::local_seed(133)
  local_items <- new.env(parent = emptyenv())
  case <- sim_vrag_case(envir = local_items)
  expect_identical(nrow(case), 1L)
  expect_identical(as.list(local_items)[vragr_fields], as.list(case))
  expect_true(all(vragr(case)$Status == "calculated"))
  expect_true(all(is.na(sim_vrag_case(prob_missing = 1, envir = NULL))))

  previous <- mget(vragr_fields, envir = .GlobalEnv, ifnotfound = list(NULL), inherits = FALSE)
  existed <- vragr_fields[vapply(vragr_fields, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
  on.exit({
    rm(list = setdiff(vragr_fields, existed), envir = .GlobalEnv)
    list2env(previous[existed], envir = .GlobalEnv)
  }, add = TRUE)
  original_globals <- mget(vragr_fields, envir = .GlobalEnv, ifnotfound = list(NULL), inherits = FALSE)
  result <- withVisible(sim_vrag_case(envir = NULL))
  expect_false(result$visible)
  expect_identical(mget(vragr_fields, envir = .GlobalEnv, ifnotfound = list(NULL), inherits = FALSE), original_globals)
  legacy <- withVisible(sim_vrag_case())
  expect_false(legacy$visible)
  expect_identical(mget(vragr_fields, envir = .GlobalEnv, inherits = FALSE), as.list(legacy$value))
})

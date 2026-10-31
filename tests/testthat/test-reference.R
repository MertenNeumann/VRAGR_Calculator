test_that("81 independent manual fixtures cover every possible total and category boundary", {
  fixture <- read.csv(file.path(root,"tests/fixtures/manual-reference.csv"),check.names=FALSE)
  scored <- vragr(fixture[vragr_fields])
  expect_equal(scored$Score,fixture$Expected_score)
  expect_equal(scored$Risk_Category,fixture$Expected_category)
  expect_equal(scored$Percentile,fixture$Expected_percentile)
  expect_equal(scored$Prob_5_years,fixture$Expected_prob_5)
  expect_equal(scored$Prob_12_years,fixture$Expected_prob_12)
  for(i in 1:12) expect_equal(scored[[paste0("Item_",i,"_score")]],fixture[[paste0("Expected_item_",i)]])
})
test_that("vectorised batch scoring also handles a large input", {
  x <- minimum_case()[rep(1,100000),]
  result <- vragr(x)
  expect_equal(nrow(result),100000)
  expect_true(all(result$Score == -34))
})

test_that("Harris Appendix C final norms agree with the implemented categories and rates", {
  reference <- read.csv(file.path(root,"tests/fixtures/manual-reference.csv"))
  norms <- read.csv(file.path(root,"tests/fixtures/harris-final-norms.csv"))
  data <- reference[match(norms$Representative_score,reference$Expected_score),vragr_fields]
  actual <- vragr(data)
  expect_equal(actual$Risk_Category,norms$Category)
  expect_equal(actual$Prob_5_years,norms$Prob_5_years)
  expect_equal(actual$Prob_12_years,norms$Prob_12_years)
})
test_that("original worked examples and the documented Appendix H discrepancy are reproducible", {
  cases <- read.csv(file.path(root,"tests/fixtures/harris-reference.csv"))
  actual <- vragr(cases[vragr_fields])
  expect_equal(actual$Score,cases$Expected_score)
  expect_equal(actual$Risk_Category,cases$Expected_category)
  expect_equal(actual$Percentile,cases$Expected_percentile)
  expect_equal(actual$Prob_5_years,cases$Expected_prob_5)
  expect_equal(actual$Prob_12_years,cases$Expected_prob_12)
  for(i in 1:12) expect_equal(actual[[paste0("Item_",i,"_score")]],cases[[paste0("Expected_item_",i)]])
  # Book C.2 gives 94 at 30; this German-manual implementation uses 93.6.
  expect_equal(actual$Percentile[1],93.6)
  expect_false(actual$Percentile[1]==94)
  # The third printed substance tick in H changes -17/category 2 to -15/3.
  expect_equal(actual$Score[3]-actual$Score[2],2)
})

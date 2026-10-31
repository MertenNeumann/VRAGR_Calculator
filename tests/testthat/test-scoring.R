test_that("manual extrema and percentile endpoints are represented", {
  low <- minimum_case(); r <- vragr(low)
  expect_equal(unname(unlist(r[1,1:6])), c(-34,1,1.2,.09,.15,0))
  high <- low; high[vragr_binary] <- "Yes"; high[c("Item_1","Item_4")] <- "No"
  high$Item_2 <- "Severe"; high$Item_5_1 <- 3; high$Item_7 <- 25
  high$Item_8_1 <- 1; high$Item_9 <- 5; high$Item_11 <- "Female14"
  high[paste0("Item_12_",1:5)] <- 2
  r <- vragr(high)
  expect_equal(unname(unlist(r[1,1:6])), c(46,9,100,.76,.87,0))
  expect_length(vragr_percentiles, 81)
})
test_that("all age boundaries and item category cutoffs follow the manual", {
  low <- minimum_case()
  for (pair in list(c(25,2),c(26,1),c(30,1),c(31,-1),c(33,-1),c(34,-2),c(38,-2),c(39,-4),c(45,-4),c(46,-7))) {
    x <- low; x$Item_7 <- pair[1]; expect_equal(vragr(x)$Item_7_score,pair[2])
  }
  for (pair in list(c(0,-2),c(1,2),c(2,3),c(3,4),c(4,4),c(5,6))) {
    x <- low; x$Item_9 <- pair[1]; expect_equal(vragr(x)$Item_9_score,pair[2])
  }
  for (j in seq_along(c("None","Moderate","Severe"))) {
    x <- low; x$Item_2 <- c("None","Moderate","Severe")[j]; expect_equal(vragr(x)$Item_2_score,c(-3,1,4)[j])
  }
  for (j in 1:4) {
    x <- low; x$Item_11 <- c("None","Only14","Other","Female14")[j]; expect_equal(vragr(x)$Item_11_score,c(-2,-1,2,3)[j])
  }
  for (n in 0:12) {
    x <- low; if(n) x[paste0("Item_10_",seq_len(n))] <- "Yes"
    expect_equal(vragr(x)$Item_10_score,c(-2,0,4,4,4,rep(5,8))[n+1])
  }
})
test_that("every Cormier-Lang weight and score band is independently checked", {
  weights <- list(c(7,3,2,5,1,5,5,2,1,1,1,5,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1),c(28,7,6,6,7,5,3,2,3,15,12,10,6,6,2,8,5,4))
  for (group in 1:2) {
    item <- c(5,8)[group]
    for (j in seq_along(weights[[group]])) {
      x <- minimum_case(); x[[paste0("Item_",item,"_",j)]] <- 1
      w <- weights[[group]][j]
      expected <- if (item==5) if(w<=2) -1 else if(w<=8) 1 else if(w<=17) 3 else 5 else if(w<=4) 2 else if(w<=18) 3 else 4
      expect_equal(vragr(x)[[paste0("Item_",item,"_score")]],expected)
    }
  }
  for (pair in list(c(0,-3),c(1,-1),c(2,-1),c(3,1),c(8,1),c(9,3),c(17,3),c(18,5))) {
    x <- minimum_case(); x$Item_5_5 <- pair[1]; expect_equal(vragr(x)$Item_5_score,pair[2])
  }
  for (pair in list(c(0,-2),c(1,2),c(4,2),c(5,3),c(18,3),c(19,4))) {
    # Weight 2 and weight 3 generate every boundary except raw 1 (not attainable).
    if(pair[1]==1) next
    x <- minimum_case(); x$Item_8_8 <- if(pair[1]%%2) (pair[1]-3)/2 else pair[1]/2
    x$Item_8_7 <- pair[1]%%2; expect_equal(vragr(x)$Item_8_score,pair[2])
  }
})
test_that("item 3 prorating rounds before scoring, including the legacy bug", {
  for(n in 0:6) {
    x <- minimum_case(); if(n) x[paste0("Item_3_",seq_len(n))] <- "Yes"
    expect_equal(vragr(x)$Item_3_score,c(-2,-2,-2,0,1,4,4)[n+1])
  }
  x <- minimum_case(); x[paste0("Item_3_",1:3)] <- "Yes"; x$Item_3_6 <- NA
  expect_equal(vragr(x)$Item_3_score,1) # 3/5*6=3.6 -> 4 -> +1 (p. 25).
  x[paste0("Item_3_",1:6)] <- NA
  expect_true(is.na(vragr(x)$Item_3_score))
  x$Item_3_1 <- "Yes"; x$Item_3_2 <- "No"
  expect_equal(vragr(x)$Item_3_score,0) # Printed example: 1/2*6=3.
  x$Item_3_3 <- "Yes"; x$Item_3_4 <- "Yes"
  expect_equal(vragr(x)$Item_3_score,4) # 4.5 -> 5 (explicit midpoint convention).
})
test_that("PCL facet score thresholds and one/two missing values are handled", {
  for(n in 0:10) {
    x <- minimum_case(); x[paste0("Item_12_",1:5)] <- as.list(c(rep(2,n%/%2), if(n%%2) 1, rep(0,5-ceiling(n/2))))
    expect_equal(vragr(x)$Item_12_score,c(-6,-3,-3,2,3,3,3,3,6,6,6)[n+1])
  }
  x <- minimum_case(); x$Item_12_5 <- NA; x$Item_12_1 <- 2
  expect_equal(vragr(x)$Item_12_score,2) # 2/4*5=2.5.
  x$Item_12_2 <- 2; x$Item_12_3 <- 2
  expect_equal(vragr(x)$Item_12_score,6) # 6/4*5=7.5.
  x$Item_12_4 <- NA; expect_true(is.na(vragr(x)$Item_12_score))
})
test_that("the published missing-item example yields 14, with distinct rates", {
  # Manual p. 20: observed sum +11, positive observed maximum +35, missing +11.
  expect_equal(vragr_total(c(2,1,0,1,3,4,1,-2,NA,NA,-2,3)),14)
  x <- minimum_case(); x$Item_1 <- "No"; x$Item_2 <- "Moderate"; x[paste0("Item_3_",1:3)] <- "Yes"
  x$Item_4 <- "No"; x$Item_5_1 <- 2; x$Item_6 <- "Yes"; x$Item_7 <- 28
  x$Item_9 <- NA; x[paste0("Item_10_",1:12)] <- NA; x$Item_12_1 <- 2; x$Item_12_2 <- 2
  r <- vragr(x); expect_equal(unname(unlist(r[1,1:6])),c(14,7,73.6,.45,.69,2))
  old <- do.call(compute_vrag,as.list(x)); expect_equal(old[[5]],69)
  expect_identical(names(formals(compute_vrag)),vragr_fields)
  expect_equal(do.call(compute_vrag,unname(as.list(x)))$Score,14)
  expect_false(anyDuplicated(names(old)) > 0)
  expect_equal(vragr_total(c(NA,vragr_min[-1])),-34)
  expect_equal(vragr_total(c(rep(NA,4),vragr_min[5:12])),-34)
  expect_true(is.na(vragr_total(c(rep(NA,5),vragr_min[6:12]))))
  # Observed -8 with Item 1 missing: -8 * 34/32 = -8.5 -> -9.
  x <- minimum_case();x$Item_1 <- NA;x$Item_5_1 <- 3;x$Item_6 <- "Yes";x$Item_7 <- 25;x$Item_11 <- "Only14"
  expect_equal(vragr(x)$Score,-9)
})
test_that("legacy synthetic data helpers retain the input schema without MASS", {
  set.seed(1)
  data <- sim_vrag_data(5,prob_missing=0)
  expect_identical(names(data),vragr_fields)
  expect_true(all(vragr(data)$Status=="calculated"))
  expect_equal(nrow(sim_vrag_data(0)),0)
  expect_error(sim_vrag_data(-1),"non-negative")
})
test_that("invalid values do not become No or zero; empty data is defined", {
  for(field in vragr_fields) {
    x <- minimum_case(); x[[field]] <- if(field %in% vragr_categories) "unrecognised" else -1
    expect_error(vragr(x),"Invalid")
    expect_equal(vragr(x,strict=FALSE)$Status,"not_calculable")
  }
  x <- minimum_case(); x$Item_7 <- 25.5; expect_error(vragr(x),"Invalid")
  x <- minimum_case(); x$Item_12_1 <- 3; expect_error(vragr(x),"Invalid")
  expect_equal(nrow(vragr(minimum_case()[FALSE,])),0)
  expect_equal(nrow(compute_vrag_data(minimum_case()[FALSE,])),0)
  x <- minimum_case(); x$Item_5_1 <- NA; expect_true(is.na(vragr(x)$Item_5_score))
})

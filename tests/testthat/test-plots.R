test_that("normal percentile views have finite coordinates, including the upper endpoint", {
  withr::local_dir(root)
  env <- new.env(parent=globalenv());source("app.R",local=env)
  for(pct in c(0,1.2,50,73.6,99.9,100)) {
    view <- env$vragr_normal_view(pct)
    expect_true(is.finite(view$marker))
    expect_true(all(is.finite(as.matrix(view$curve))))
    expect_true(all(view$shaded$Z<=view$marker))
  }
  expect_equal(env$vragr_normal_view(50)$marker,0)
  expect_equal(env$vragr_normal_view(100)$marker,3.5)
  cases <- read.csv("tests/fixtures/manual-reference.csv")
  for(index in c(1,49,81)) {
    result <- vragr(cases[index,vragr_fields])
    for(type in c("empirical","normal")) {
      plot <- env$vragr_result_plot(result,type,"de")
      expect_no_warning(built <- ggplot2::ggplot_build(plot))
      for(layer in built$data) {
        numbers <- layer[vapply(layer,is.numeric,logical(1))]
        expect_false(any(vapply(numbers,function(x) any(is.infinite(x)),logical(1))))
      }
    }
  }
})

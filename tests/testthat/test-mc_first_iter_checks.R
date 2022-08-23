test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)

  if (x2 == 5){
    stop("x2 can't be 5!")
  }
  return(list(mean = stat))
}


param_list <- list(param = seq(from = 0, to = 1, by = 0.5), n = 100,
                   x1 = 1:2, x2 = 1)



testthat::test_that("quick checks work", {
  testthat::expect_error(
    future_mc(fun = test_func, repetitions = 1000, param_list = param_list),
    regexp = "Function error:"
  )
})

out <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)
invisible({
  out.plot <- plot(out)
  out.summary <- summary(out)
  out.latex <- tidy_mc_latex(out, sum_funs = list(mean = mean))
})

testthat::test_that("Class of the outputs",{

  testthat::expect_type(object = out, type = "list")
  testthat::expect_type(out.plot, "list")
  testthat::expect_s3_class(out.plot[[1]], "gg")
  testthat::expect_s3_class(out.summary, "summary.mc")
  testthat::expect_s3_class(out.latex, "knitr_kable")
})


testthat::test_that("Functions print",{

  testthat::expect_output({
    out <- future_mc(
      fun = test_func, repetitions = 1000, param_list = param_list)
  })
  testthat::expect_output({print(out)})
  testthat::expect_output(print(out.summary))
  testthat::expect_output(print(out.latex))
})


testthat::test_that("Number of combinations for summary functions", {
  res <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)

  testthat::expect_identical(nrow(res$parameter),
                             length(summary(res, sum_funs = list(mean = mean,
                                                                 sd = sd,
                                                                 test = table))))

})



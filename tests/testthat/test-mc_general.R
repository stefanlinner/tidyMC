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

out <- future_mc(fun = test_func, repetitions = 10, param_list = param_list)
invisible({
  out.plot <- plot(out)
  out.summary <- summary(out)
  out.latex <- tidy_mc_latex(x = summary(out), which_out = "mean")
})



testthat::test_that("Errors check work", {
  param_list <- list(param = seq(from = 0, to = 1, by = 0.5), n = 100,
                     x1 = 1:2, x2 = 5)
  testthat::expect_error( # Test function error
    future_mc(fun = test_func, repetitions = 10, param_list = param_list),
    regexp = "Function error:"
  )
  testthat::expect_error({ # List of functions the summary has to have names
    summary(out, sum_funs = list(mean))
  }, regexp = "Must have names")
  testthat::expect_error({ # The results have to be named
    param_list2 <- list(param = seq(from = 0, to = 1, by = 0.5), n = 100,
                       x1 = 1:2, x2 = 1)
    test_func2 <- function(param = 0.1, n = 100, x1 = 1, x2 = 1){

      data <- rnorm(n, mean = param) + x1 + x2
      stat <- mean(data)
      stat_2 <- var(data)

      if (x2 == 5){
        stop("x2 can't be 5!")
      }
      return(list(stat, stat2))
    }

    future_mc(fun = test_func2, repetitions = 10, param_list = param_list)
  })

  testthat::expect_error(tidy_mc_latex(summary(out),
                                       repetitions_set = c(10, 20000),
                                       which_out = "mean"))
  testthat::expect_error({ # A variable from the global environment cannot be called
    rand_var <- rnorm(1)
    test_func2 <- function(param = 0.1, n = 100, x1 = 1, x2 = 1){

      data <- rnorm(n, mean = param) + x1 + x2 + rand_var
      stat <- mean(data)
      stat_2 <- var(data)

      if (x2 == 5){
        stop("x2 can't be 5!")
      }
      return(list(stat, stat2))
    }
    future_mc(fun = test_func2, repetitions = 10, param_list = param_list)
  })

})


testthat::test_that("Class of the outputs",{

  testthat::expect_type(object = out, type = "list")
  testthat::expect_type(out.plot, "list")
  testthat::expect_s3_class(out.plot[[1]], "gg")
  testthat::expect_s3_class(out.summary, "summary.mc")
  testthat::expect_s3_class(out.latex, "knitr_kable")
})


testthat::test_that("Functions print",{
  testthat::expect_message({out <- future_mc(
    fun = test_func, repetitions = 1000, param_list = param_list)})
  testthat::expect_output({print(out)})
  testthat::expect_output(print(out.summary))
  testthat::expect_output(print(out.latex))
})


testthat::test_that("Number of results coincides", {
  testthat::expect_identical(nrow(out$parameter),
                             length(summary(out,
                                            sum_funs = list(mean = mean))))
  testthat::expect_identical(nrow(out$parameter),
                             length(summary(out)))
  testthat::expect_identical(length(out$nice_names), nrow(out$parameter))

  testthat::expect_named(summary(out))
})



test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)

  if (x2 == 5){
    stop("x2 can't be 5!")
  }
  return(list(mean = stat))
}


param_list <- list(param = seq(from = 0, to = 1, by = 0.5), n = c(100,1000),
                   x1 = 1:2, x2 = 1)

out <- future_mc(fun = test_func, repetitions = 10, param_list = param_list)
invisible({
  out.plot <- plot(out)
  out.summary <- summary(out)
  out.latex <- tidy_mc_latex(x = summary(out), which_out = "mean")
})


# The number of repetitions shouldn't be bigger than the parameter list
testthat::expect_error(tidy_mc_latex(summary(out),
                                     repetitions_set = c(10, 20000),
                                     which_out = "mean"))

# The object should have the class knitr_kable for further customizations
testthat::expect_s3_class(tidy_mc_latex(
  x = summary(out),
  repetitions_set = c(100, 1000)
), class = "knitr_kable")

# The object should be visible
testthat::expect_visible(tidy_mc_latex(
  x = summary(out),
  repetitions_set = c(100, 1000)
))

# Having other summary statistics
testthat::expect_error(tidy_mc_latex(summary(out),
                                     repetitions_set = c(100, 1000),
                                     which_out = "sd"))


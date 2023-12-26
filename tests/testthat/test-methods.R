test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)

  if (x2 == 5){
    stop("x2 can't be 5!")
  }
  return(list(mean = stat))
}


param_list <-
  list(
    param = seq(from = 0, to = 1, by = 0.5),
    n = 100,
    x1 = 1:2,
    x2 = 1
  )

out <-
  future_mc(
    fun = test_func,
    repetitions = 10,
    param_list = param_list
  )

sum_mc <-
  summary(out)

plot_mc <-
  plot(out, plot = FALSE)

plot_sum_mc <-
  plot(sum_mc, plot = FALSE)

testthat::test_that("output format of summary is correct", {

  testthat::expect_output(str(sum_mc), "List of 6")

  testthat::expect_output(str(sum_mc[[1]]), "List of 2")
  testthat::expect_output(str(sum_mc[[2]]), "List of 2")
  testthat::expect_output(str(sum_mc[[3]]), "List of 2")
  testthat::expect_output(str(sum_mc[[4]]), "List of 2")
  testthat::expect_output(str(sum_mc[[5]]), "List of 2")
  testthat::expect_output(str(sum_mc[[6]]), "List of 2")

  testthat::expect_identical(names(sum_mc), out$nice_names)

  testthat::expect_output(str(sum_mc[[1]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[1]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_output(str(sum_mc[[2]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[2]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_output(str(sum_mc[[3]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[3]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_output(str(sum_mc[[4]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[4]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_output(str(sum_mc[[5]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[5]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_output(str(sum_mc[[6]][[1]][[1]]),
                          "num",
                          fixed = TRUE)
  testthat::expect_output(str(sum_mc[[6]][[1]][[2]]),
                          "num [1:10]",
                          fixed = TRUE)

  testthat::expect_identical(names(sum_mc[[1]]), "mean")
  testthat::expect_identical(names(sum_mc[[2]]), "mean")
  testthat::expect_identical(names(sum_mc[[3]]), "mean")
  testthat::expect_identical(names(sum_mc[[4]]), "mean")
  testthat::expect_identical(names(sum_mc[[5]]), "mean")
  testthat::expect_identical(names(sum_mc[[6]]), "mean")

  testthat::expect_identical(names(sum_mc[[1]][[1]]),
                             c("mean", "mean_over_reps"))
  testthat::expect_identical(names(sum_mc[[2]][[1]]),
                             c("mean", "mean_over_reps"))
  testthat::expect_identical(names(sum_mc[[3]][[1]]),
                             c("mean", "mean_over_reps"))
  testthat::expect_identical(names(sum_mc[[4]][[1]]),
                             c("mean", "mean_over_reps"))
  testthat::expect_identical(names(sum_mc[[5]][[1]]),
                             c("mean", "mean_over_reps"))
  testthat::expect_identical(names(sum_mc[[6]][[1]]),
                             c("mean", "mean_over_reps"))

})


testthat::test_that("output format of plot.mc is correct", {

  testthat::expect_output(str(plot_mc), "List of 1")
  testthat::expect_s3_class(plot_mc[[1]], "gg")

  testthat::expect_identical(names(plot_mc), "mean")

})


testthat::test_that("output format of summary.plot.mc is correct", {

  testthat::expect_output(str(plot_mc), "List of 1")
  testthat::expect_s3_class(plot_mc[[1]], "gg")

  testthat::expect_identical(names(plot_mc), "mean")

})

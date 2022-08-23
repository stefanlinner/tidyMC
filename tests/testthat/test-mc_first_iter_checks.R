test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)

  if (x2 == 5){
    stop("x2 can't be 5!")
  }

  return(list(mean = stat, sd = stat_2))
}


param_list <- list(param = seq(from = 0, to = 1, by = 0.5), n = 100,
                   x1 = 1:2, x2 = 1:5)


testthat::test_that("quick checks work", {

  testthat::expect_error(
    future_mc(fun = test_func, repetitions = 1000, param_list = param_list),
    regexp = "Function error:"
  )


})


param_list <- list(param = seq(from = 0, to = 1, by = 0.2), n = 20,
                   x1 = 1:2, x2 = 1:2)

testthat::test_that("Class of the output",{
  out <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)
  testthat::expect_type(object = out, type = "list")
})

# I don't get why there is an error here





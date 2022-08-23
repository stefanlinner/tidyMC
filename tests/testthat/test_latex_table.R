test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)
  test <- LETTERS[sample(1:26, 1)]

  if(x3 == 0 & x4 == 0){
    next
  }

  if (x2 == 5){
    stop("x2 can't be 5!")
  }

  return(list(mean = stat, sd = stat_2, test = test))
}


param_list <- list(param = seq(from = 0, to = 2, by = 0.5),
                   n = c(10,100,1000), x1 = 1, x2 = 10)




testthat::test_that("The output of latex table is printed", {
  res <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)



  testthat::expect_output({res <- tidy_mc_latex(x = summary(res), repetitions_set = c(10, 500, 1000),
                                                caption = "Nice table")})

})











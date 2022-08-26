# Nice OLS example for the report

# Yt = B0 + B1*x1 + B2*x2 + e

devtools::load_all()


ols_test <-
  function(b0, b1, b2, n, sigma2, param_x1, param_x2, inc_x2){

    # generation of data
    x1 <- rnorm(n = n, mean = param_x1[1], sd = param_x1[2])
    x2 <- rnorm(n = n,  mean = param_x2[1], sd = param_x2[2])
    e <- rnorm(n, sd = sqrt(sigma2))
    y <- b0 + b1*x1 + b2*x2 + e

    if (inc_x2 == 0){
      x2 <- x2 * inc_x2
    }

    # application of method
    estim <- lm(y ~ x1 + x2)

    # evaluation of the result for a single repetition and parameter combination
    out <- list(B0 = estim$coefficients[1],
                B1 = estim$coefficients[2],
                B2 = estim$coefficients[3],
                s2 = var(estim$residuals))
    return(out)
  }


param_list_ols <-
  list(n = c(100, 200, 300))

ols <- future_mc(fun = ols_test, repetitions = 10000, param_list = param_list_ols,
                 b0 = 1, b1 = 4, b2 = 5, param_x1 = c(1,2), param_x2 = c(3,4),
                 sigma2 = 3, inc_x2 = 1)

tidy_mc_latex(summary(ols), repetitions_set = c(10, 10000),
                           column_names = c("$\\beta_0$", "$\\beta_1$",
                                            "$\\beta_2$", "$s^2$"))
invisible(ols_plots <- plot(ols))
test$B1

# Irrelevant variable

ols_irr <- future_mc(fun = ols_test, repetitions = 10000,
                     param_list = param_list_ols, b0 = 1, b1 = 4,
                     b2 = 0, param_x1 = c(1,2), param_x2 = c(3,4),
                     sigma2 = 3, inc_x2 = 1)

ols_irr.latex <- tidy_mc_latex(summary(ols_irr), repetitions_set = c(10, 10000),
                               column_names = c("$\\beta_0$", "$\\beta_1$",
                                                "$\\beta_2$", "$s^2$"))








# Bootstrap function is not what is intended but lets do it


b0 <- 2
b1 <- 1
N <- 10000
n <- 1000
sigma2 <- 3
x1 <- rnorm(N, mean = 3, 2)
e <- rnorm(N, sd = sqrt(sigma2))
y <- b0 + x1*b1 + e
pop_data.test <- data.frame(inter = rep(1, N),
                      X1 = x1,
                      Y = y,
                      eps = e)


test_boot <- function(n, pop_data){
  id <- sample(x = 1:10000, size = n, replace = FALSE)
  sam_data <- pop_data[id,]
  boot_data <- sam_data[sample(x = 1:nrow(sam_data),
                               size = n, replace = TRUE), -c(1, 4)]

  estim <- lm(Y ~ X1, data = boot_data)


  return(list(B0 = estim$coefficients[1],
              B1 = estim$coefficients[2]))

}

test_boot(50, pop_data = pop_data.test)

param_list_boot <- list(n = 1000)

MC_boot <- future_mc(fun = test_boot, repetitions = 1000, param_list = param_list_boot,
                     parallelisation_plan = NULL,
                     parallelisation_options = NULL,
                     check = TRUE,
                     parallel = TRUE,
                     pop_data = pop_data.test)


# We can't include variables that are defined outside the function


MC_boot.summary <- summary(MC_boot)


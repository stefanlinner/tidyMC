# Nice OLS example for the report

# Yt = B0 + B1*x1 + B2*x2 + e

devtools::load_all()


ols_test <- function(b0, b1, b2, n,
                     sigma2){
  # Gen x independently
  x1 <- rnorm(n = n, mean = 1, sd = 5)
  x2 <- rnorm(n = n,  mean = 2, sd = 1)
  # Generate error term
  e <- rnorm(n, sd = sqrt(sigma2))

  # Gen y dependently
  y <- b0 + b1*x1 + b2*x2 + e


  # Estimate OLS on the sample
estim <- lm(y ~ x1 + x2)

  return(list(B0 = estim$coefficients[1],
              B1 = estim$coefficients[2],
              B2 = estim$coefficients[3],
              s2 = var(estim$residuals)))
}


param_list <- list(b0 = 1, b1 = 4, b2 = 5, n = c(10, 100, 1000, 10000),
                   sigma2 = 2)

ols <- future_mc(fun = ols_test, repetitions = 10000, param_list = param_list)

ols.latex <- tidy_mc_latex(summary(ols), repetitions_set = c(10, 10000))


ols.plot_n10_b1 <- plot(x = ols, parameter_comb = list(n = 10))$B1

ols.plot_n10_b1 +
  ggplot2::theme_minimal() +
  # ggplot2::ggtitle("Beta 1 results for 10 MC repetitions") +
  ggplot2::labs(title = "Beta 1 results for 10 MC repetitions")


ols.plot_n10_s2 <- plot(x = test1, parameter_comb = list(n = 10))$s2

ols.plot_n10_s2 +
  ggplot2::theme_minimal() +
  # ggplot2::ggtitle("Beta 1 results for 10 MC repetitions") +
  ggplot2::labs(title = "s2 results for 10 MC repetitions")

ols.plot_n10_b1 <- plot(x = test1, parameter_comb = list(n = 10000))$B1

ols.plot_n10k_b1 +
  ggplot2::theme_minimal() +
  # ggplot2::ggtitle("Beta 1 results for 10 MC repetitions") +
  ggplot2::labs(title = "Beta 1 results for 10000 MC repetitions")


ols.plot_n10k_s2 <- plot(x = test1, parameter_comb = list(n = 10000))$s2

ols.plot_n10k_s2 +
  ggplot2::theme_minimal() +
  # ggplot2::ggtitle("Beta 1 results for 10 MC repetitions") +
  ggplot2::labs(title = "s2 results for 10000 MC repetitions")


# Irrelevant variable


ols_test <- function(b0, b1, b2, n,
                     sigma2){
  # Gen x independently
  x1 <- rnorm(n = n, mean = 1, sd = 5)
  x2 <- rnorm(n = n,  mean = 2, sd = 1)
  # Generate error term
  e <- rnorm(n, sd = sqrt(sigma2))

  # Gen y dependently
  y <- b0 + b1*x1 + b2*x2 + e


  # Estimate OLS on the sample
  estim <- lm(y ~ x1 + x2)

  estim.summary <- summary(estim)


  return(list(B2 = estim$coefficients[3],
              Pval_B2 = ifelse(estim.summary$coefficients[3,4]<0.05, "Reject", "Not reject"),
              s2 = var(estim$residuals)))
}

param_list <- list(b0 = 1, b1 = 5, b2 = 0, n = c(10, 100, 1000, 10000),
                   sigma2 = 4)

ols_irr <- future_mc(fun = ols_test, repetitions = 10000, param_list = param_list)

ols_irr.summary <- summary(ols_irr)

ols_irr.latex <- tidy_mc_latex(x)











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


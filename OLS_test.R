# Nice OLS example for the report

# Yt = B0 + B1*x1 + B2*x2 + e

b0 <- 2
b1 <- 1
b2 <- 3
n <- 100

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

  estim$coefficients

  return(list(B0 = estim$coefficients[1],
              B1 = estim$coefficients[2],
              B2 = estim$coefficients[3],
              sig2 = var(estim$residuals)))
}


param_list <- list(b0 = 0:1, b1 = 1, b2 = 3, n = c(10, 100, 1000, 10000),
                   sigma2 = 1)

test1 <- future_mc(fun = ols_test, repetitions = 5000, param_list = param_list)




ols_test_error <- function(b0, b1, b2, n,
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

  estim$coefficients

  return(list(b0 = estim$coefficients[1],
              b1 = estim$coefficients[2],
              b2 = estim$coefficients[3],
              sigma2 = var(estim$residuals)))
}
test1 <- future_mc(fun = ols_test_error, repetitions = 5000, param_list = param_list)
# Name of the results cannot be the same as the name of the inputs



summary(test1)

plot(test1)

tidy_mc_latex(summary(test1), repetitions_set = c(1000, 2500, 5000))



# Bootstrap function is not what is inteded but lets do it


b0 <- 2
b1 <- 1
N <- 10000
sigma2 <- 3
x1 <- rnorm(N, mean = 3, 2)
e <- rnorm(N, sd = sqrt(sigma2))
y <- b0 + x1*b1 + e
pop_data <- data.frame(inter = rep(1, N),
                      X1 = x1,
                      Y = y,
                      eps = e)


test_boot <- function(n_boot, n, pop_data){
  id <- sample(x = 1:10000, size = n, replace = FALSE)
  sam_data <- pop_data[id,]
  boot_data <- sam_data[sample(x = 1:nrow(sam_data),
                               size = n_boot, replace = TRUE), -c(1, 4)]

  estim <- lm(Y ~ X1, data = boot_data)


  return(list(B0 = estim$coefficients[1],
              B1 = estim$coefficients[2]))

}


param_list_boot <- list(n_boot = 1000, n = 1000)

MC_boot <- future_mc(fun = test_boot, repetitions = 500, param_list = param_list_boot, pop_data = pop_data)

# We can't include variables that are defined outside the function




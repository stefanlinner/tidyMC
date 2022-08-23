### First functions ####
#
# library(tidyverse)
# library(parallelly)
# library(codetools)
# library(MonteCarlo)
# library(furrr)


parallelly::availableCores()
#
#
# cores_number <- 4
# repetitions <- 10

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


param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
                   x1 = 1:2, x2 = 2:4)

devtools::load_all()

set.seed(101)

test1 <- future_mc(fun = test_func,
                   repetitions = 1000,
                   param_list = param_list,
                   x3 = 6, x4 = 1, check = TRUE)

test1
plot(test1)
plot(test1, which = test1$setups[1:2])
plot(test1, join = test1$setups)


# New feature: Now we can select the parameters we want to choose and filter for
# the table. We need a named list with the parameters for the filter.

test_latex <- tidy_mc_latex(object = test1)

test_latex <- tidy_mc_latex(object = test1, repetitions_set = c(10, 500, 1000),
                            sum_funs = list(mean = max, sd = min, test = table))

test_latex <- tidy_mc_latex(object = test1, repetitions_set = c(10, 500, 1000),
                            sum_funs = list(mean = mean, sd = mean, test = table),
                            parameter_comb = list(param = 0.5))



# Note: You can modify the ggplots by yourself

test.plot <- plot(test1)

test.plot$mean + ggplot2::theme_minimal()


# default summary function --> summary (compatible with any data type)
summary(test1)

# user_defined summary function for different results
summary(test1, sum_funs = list(mean = mean, sd = sd, test = table))

# user_defined summary function for different results and different setups
sum_funcs <- list(
  list(
    mean = mean, sd = sd, test = table
  ),
  list(
    mean = summary, sd = summary, test = table
  ),
  list(
    mean = max, sd = min, test = summary
  )
)

names(sum_funcs) <- unique(test1$output$params)

summary(test1, sum_funs = sum_funcs)


plot(summary(test1))
plot(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)), which = test1$setups[1:2])
plot(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)), join = test1$setups)
plot(summary(test1, sum_funs = sum_funcs))

# ggplot2 informative legends with grid-package, gtable (?)





# Do the latex tables from summary output




test_func<-function(n,loc,scale){
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  decision<-abs(stat)>1.96
  return(list("decision"=decision))
}

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
erg<-MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)

# test <- bench::mark({MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)},
#             {future_mc(func = test_func, repetitions = 250, param_list = param_list)}, check = FALSE)

print(erg)

summary(erg)



# summary functions -> list of function for each result in list (-> single function --> used for all, default --> summary())
# What to do with the furrr.options?, seed argument user-defined?
# Nice output -> look in tidyverse package



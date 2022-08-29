### First functions ####
#
# library(tidyverse)
# library(parallelly)
# library(codetools)
# library(MonteCarlo)
# library(furrr)


# parallelly::availableCores()
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


# test_func_1out <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){
#
#   data <- rnorm(n, mean = param) + x1 + x2
#   stat <- mean(data)
#   stat_2 <- var(data)
#   test <- LETTERS[sample(1:26, 1)]
#
#   if(x3 == 0 & x4 == 0){
#     next
#   }
#
#   if (x2 == 5){
#     stop("x2 can't be 5!")
#   }
#
#   return(list(mean = stat))
# }
# test1 <- future_mc(fun = test_func_1out,
#                    repetitions = 1000,
#                    param_list = param_list,
#                    x3 = 6, x4 = 1, check = TRUE)



param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
                   x1 = 1, x2 = 2)

devtools::load_all()

set.seed(101)

test1 <- future_mc(fun = test_func,
                   repetitions = 5000,
                   param_list = param_list,
                   x3 = 6, x4 = 1, check = TRUE)


test1.plot <- plot(test1)
plot(test1, which_setup = test1$nice_names[1:2])
plot(test1, join = test1$nice_names)
plot(test1, parameter_comb = list(param = c(0, 0.5)))



test1.plot <- plot(test1, plot = FALSE)
test1.plot$mean

# To do the table we need to give the sum_funs object and provide a function for
# each result. Functions that don't give the nice output of just one scalar and
# the time series are ignored from the table.


test_latex <- tidy_mc_latex(x = summary(test1), repetitions_set = c(10, 500, 1000))
test_latex <- tidy_mc_latex(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)))

sum_funcs <- list(
  list(
    mean = mean, sd = sd, test = table
  ),
  list(
    mean = mean, sd = summary, test = table
  ),
  list(
    mean = max, sd = min, test = summary
  )
)

names(sum_funcs) <- unique(test1$output$params)

summary(test1, sum_funs = sum_funcs)

test_latex <- tidy_mc_latex(summary(test1, sum_funs = sum_funcs))

test_latex <- tidy_mc_latex(x = summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)),
                            repetitions_set = c(10, 500, 1000),
                            parameter_comb = list(param = c(0,0.5)), which_stat = "mean")

test_latex <- tidy_mc_latex(x = summary(test1),
                            repetitions_set = c(10, 500, 1000),
                            which_setup = unique(test1$output$params)[1:2], which_stat = "mean")



tidy_mc_latex(x = summary(test1), repetitions_set = c(10,500, 1000))

# Note: You can modify the ggplots by yourself

test.plot <- plot(test1)


test.plot$mean +
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = 3)


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
plot(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)), which_setup = test1$nice_names[1:2])
plot(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)), join = test1$nice_names)
plot(summary(test1, sum_funs = sum_funcs))
test_summary_plot <- plot(summary(test1, sum_funs = list(mean = mean, sd = sd, test = table)), parameter_comb = list(param = c(0,0.5)))


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



# Performance

test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)
  test <- LETTERS[sample(1:26, 1)]

  # Sys.sleep()

  if(x3 == 0 & x4 == 0){
    next
  }

  if (x2 == 5){
    stop("x2 can't be 5!")
  }

  return(list(mean = stat, sd = stat_2, test = test))
}

param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.2),
                   x1 = 1, x2 = c(2,3,4))

param_table <- expand.grid(param_list)
n_params <- nrow(param_table)


repetitions <- 1000
parallelisation_options <- list(seed = TRUE)


library(furrr)
library(MonteCarlo)
library(bench)
devtools::load_all()

# Comparison of non-parallel version MonteCarlo vs. future_mc
speed_result1 <- bench::mark({
  results <- MonteCarlo(func=test_func, nrep=repetitions, param_list=param_list)
},
{
  param_table_reps <-
    purrr::map_dfr(
      seq_len(repetitions),
      function(x) param_table
    )
  results_list <-
    furrr::future_pmap(
      .l = param_table_reps,
      .f = test_func,
      .options = do.call(furrr::furrr_options, parallelisation_options),
      .progress = TRUE
    )
}, check = FALSE)
speed_result1$median


speed_result2 <- bench::mark({
  results <- MonteCarlo(
    func=test_func,
    nrep=repetitions,
    param_list=param_list
  )
},
{
  results <- future_mc(
    test_func,
    repetitions = repetitions,
    param_list = param_list,
    check = FALSE,
    parallel = FALSE,
    parallelisation_options = list(seed = TRUE)
  )
}, check = FALSE)
speed_result2$median


# Parallel comparison of MonteCarlo and future_mc

cores <- parallelly::availableCores()

plan(multisession, workers = cores)

speed_results3 <- bench::mark({
  results <- MonteCarlo(func=test_func, nrep=repetitions, param_list=param_list, ncpus = cores) # Why is it slower??
},
{
  param_table_reps <-
    purrr::map_dfr(
      seq_len(repetitions),
      function(x) param_table
    )
  results_list <-
    furrr::future_pmap(
      .l = param_table_reps,
      .f = test_func,
      .options = do.call(furrr::furrr_options, parallelisation_options),
      .progress = TRUE
    )
}, check = FALSE)
speed_results3$median

speed_results4 <- bench::mark({
  results <-
    MonteCarlo(
      func=test_func,
      nrep=repetitions,
      param_list=param_list,
      ncpus = cores # Why is it slower??
    )
},
{
  results <- future_mc(
    test_func,
    repetitions = repetitions,
    param_list = param_list,
    check = FALSE,
    parallel = TRUE,
    parallelisation_options = list(seed = TRUE),
    parallelisation_plan = list(strategy = multisession, workers = cores)
  )
}, check = FALSE)
speed_results4$median


# Comparison of different parallelizations

plan(multisession(workers = 8))

speed_results5 <- bench::mark({
  # parallelise over parameters
  results_list <-
    furrr::future_map(
      .x = 1:n_params,
      .f = function(.x){
        purrr::map_df(
          1:repetitions,
          .f = function(.y) {
            purrr::pmap_dfr(param_table[.x,], test_func)
          }
        )
      },
      .progress = TRUE,
      .options = do.call(furrr::furrr_options, parallelisation_options)
    )
},
{
  # Parallel over reps
  results_list <-
    furrr::future_map(
      .x = 1:repetitions,
      .f = function(.x){
        purrr::map_df(
          1:nrow(param_table),
          .f = function(.y) {
            purrr::pmap_dfr(param_table[.y,], test_func)
          }
        )
      },
      .progress = TRUE,
      .options = do.call(furrr::furrr_options, parallelisation_options)
    )
},
{
  # Parallel over both
  param_table_reps <-
    purrr::map_dfr(
      seq_len(repetitions),
      function(x) param_table
    )
  results_list <-
    furrr::future_pmap(
      .l = param_table_reps,
      .f = test_func,
      .options = do.call(furrr::furrr_options, parallelisation_options),
      .progress = TRUE
    )
}, check = FALSE)
speed_results5$median


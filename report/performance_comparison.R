library(furrr)
library(MonteCarlo)
library(bench)
library(tidyMC)

test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)

  if (x2 == 5){
    stop("x2 can't be 5!")
  }

  return(list(mean = stat, var = stat_2))
}

grid <-
  expand.grid(list(
    reps = c(100, 1000),
    n_params = c(1, 10, 100),
    cores = c(1, 4)
  ))

comparison_results_mc <-
  bench::press(
    .grid = grid,
    {
      param_list <- list(param = seq(from = 0, to = 1, length.out = n_params), x1 = 1)

      bench::mark({
        results <-
          MonteCarlo(
            func=test_func,
            nrep=reps,
            param_list=param_list,
            ncpus = cores
          )
      },
      {
        results <- future_mc(
          test_func,
          repetitions = reps,
          param_list = param_list,
          check = FALSE,
          parallel = TRUE,
          parallelisation_options = list(seed = TRUE),
          parallelisation_plan = list(strategy = multisession, workers = cores)
        )
      },
      check = FALSE,
      time_unit = "s"
      )
    }
  )

comparison_results_mc$alg <- rep(c("MonteCarlo", "tidyMC"), 12)
print(comparison_results_mc[, c("reps", "n_params", "cores", "median", "mem_alloc")], n = 24)


comparison_results_diff_approach <-
  bench::press(
    .grid = grid,
    {
      parallelisation_options <- list(seed = TRUE)
      param_list <- list(param = seq(from = 0, to = 1, length.out = n_params), x1 = 1)
      param_table <- expand.grid(param_list)

      bench::mark({
        # parallelise over parameters
        future::plan(multisession, workers = cores)
        results_list <-
          furrr::future_map(
            .x = 1:n_params,
            .f = function(.x){
              purrr::map_df(
                1:reps,
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
        future::plan(multisession, workers = cores)
        results_list <-
          furrr::future_map(
            .x = 1:reps,
            .f = function(.x){
              purrr::map_df(
                1:n_params,
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
        future::plan(multisession, workers = cores)
        param_table_reps <-
          purrr::map_dfr(
            seq_len(reps),
            function(x) param_table
          )
        results_list <-
          furrr::future_pmap(
            .l = param_table_reps,
            .f = test_func,
            .options = do.call(furrr::furrr_options, parallelisation_options),
            .progress = TRUE
          )
      },
      check = FALSE,
      time_unit = "s"
      )
    }
  )

comparison_results_diff_approach$alg <- rep(c("par_over_param", "par_over_reos", "tidyMC"), 12)
print(comparison_results_diff_approach[, c("reps", "n_params", "cores", "median", "mem_alloc")], n = 36)

# save.image("report/speed_result.RData")
load("report/speed_result.RData")

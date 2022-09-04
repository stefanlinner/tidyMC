library(furrr)
library(MonteCarlo)
library(bench)
library(tidyMC)

test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, sleep_time = 0){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- sd(data)

  Sys.sleep(sleep_time)

  return(list(mean = stat, sd = stat_2))
}

grid <-
  expand.grid(list(
    reps = c(10,100,100,10000),
    n_params = c(1,10,100,1000),
    # sleep_times = c(0, 0.05),# c(0,0.2),
    cores = c(1,2,3,4)
  ))

# comparison_results1 <-
#   bench::press(
#     .grid = grid,
#     {
#       param_list <- list(param = seq(from = 0, to = 1, length.out = n_params), x1 = 1)
#
#       bench::mark({
#         monte_carlo <-
#           MonteCarlo(
#             func=test_func,
#             nrep=reps,
#             param_list=param_list,
#             ncpus = cores
#           )
#       },
#       {
#         tidy_mc <-  future_mc(
#           test_func,
#           repetitions = reps,
#           param_list = param_list,
#           check = FALSE,
#           parallel = TRUE,
#           parallelisation_options = list(seed = TRUE),
#           parallelisation_plan = list(strategy = multisession, workers = cores)
#         )
#       },
#       check = FALSE,
#       time_unit = "s"
#       )
#     }
#   )


MonteCarlo_vs_tidyMC <- function(reps, cores){

  test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){

    data <- rnorm(n, mean = param) + x1 + x2
    stat <- mean(data)
    stat_2 <- var(data)
    test <- LETTERS[sample(1:26, 1)]

    # Sys.sleep() ? Let's do it

    if(x3 == 0 & x4 == 0){
      next
    }

    if (x2 == 5){
      stop("x2 can't be 5!")
    }

    return(list(mean = stat, sd = stat_2, test = test))
  }

  combs <- c("1 combination", "10 combinations", "100 combinations")

  pars <- list("1 combination" = list(n = c(1e2), param = 1,
                                       x1 = 1, x2 = 2),
               "10 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.2, to = 1, by = 0.2),
                                        x1 = 1, x2 = 2),
               "100 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.04, to = 1, by = 0.04),
                                         x1 = 1, x2 = c(2,4))
  )

  # ,
  # "1000 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.01, to = 1, by = 0.01),
  #                            x1 = 1, x2 = seq(2,10, 2))

  out <- data.frame(
    kind = rep(c("time", "memory"), each = length(reps)*length(cores)*length(combs)),
    combs = rep(rep(combs, each = length(reps)*length(cores)),2),
    cores = rep(rep(cores, length(reps)*length(combs)),2),
    MC_reps = rep(rep(rep(reps, each = length(cores)), length(combs)),2),
    tidyMC = rep(rep(NA_real_, length(reps)*length(cores)*length(combs)),2),
    MonteCarlo = rep(rep(NA_real_, length(reps)*length(cores)*length(combs)),2)
  )

overall_progres <- length(pars) * length(reps) * length(cores) * 1
count <- 1

  for (comb in combs){
    for(core in cores){
      for(n in reps){

        speed_results <- bench::mark({
          results <-
            MonteCarlo(
              func=test_func,
              nrep=n,
              param_list=pars[[comb]],
              ncpus = core # Why is it slower??
            )
        },
        {
          results <- future_mc(
            test_func,
            repetitions = n,
            param_list = pars[[comb]],
            check = FALSE,
            parallel = TRUE,
            parallelisation_options = list(seed = TRUE),
            parallelisation_plan = list(strategy = multisession, workers = core)
          )
        }, check = FALSE,
        filter_gc = TRUE,
        time_unit = "s",
        iterations = 1)

        out$MonteCarlo[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "time"] <- speed_results$median[2]
        out$tidyMC[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "time"] <- speed_results$median[3]

        out$MonteCarlo[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "memory"] <- speed_results$mem_alloc[2]
        out$tidyMC[out$cores == core & out$MC_reps == n & out$combs == comb& out$kind == "memory" ] <- speed_results$mem_alloc[3]
        if (count == 1){
          print(paste(count/overall_progres, "% done"))
        }
        count <- count + 1
      }
    }
  }

  return(out)
}


res <- MonteCarlo_vs_tidyMC(reps = c(100, 500, 1000), cores = c(1,2,3))

# mc_vs_tidyMC_plot <- MonteCarlo_vs_tidyMC(reps = c(1e2,1e3),
#                                           cores = c(1,2,3))
#
# mc_vs_tidyMC_plot_time <- mc_vs_tidyMC_plot  %>%
#   filter(kind == "time") %>%
#   pivot_longer(cols = c(tidyMC, MonteCarlo)) %>%
#   ggplot(aes(x = MC_reps, y = value, col = name)) +
#   geom_line() +
#   facet_grid(cols = vars(cores),
#              rows = vars(combs)) +
#   scale_x_continuous(trans = "log10")
#
# mc_vs_tidyMC_plot_memory <- mc_vs_tidyMC_plot %>%
#   filter(kind == "memory") %>%
#   pivot_longer(cols = c(tidyMC, MonteCarlo)) %>%
#   ggplot(aes(x = MC_reps, y = value, col = name)) +
#   geom_line() +
#   facet_grid(cols = vars(cores),
#              rows = vars(combs)) +
#   scale_x_continuous(trans = "log10")


martin_vs_tidyMC <- function(reps, cores){

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

  combs <- c("1 combination", "10 combinations", "100 combinations", "1000 combinations")

  pars <- list("1 combinations" = list(n = c(1e2), param = 1,
                                       x1 = 1, x2 = 2),
               "10 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.2, to = 1, by = 0.2),
                                        x1 = 1, x2 = 2),
               "100 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.04, to = 1, by = 0.04),
                                         x1 = 1, x2 = c(2,4)),
               "1000 combinations" = list(n = c(1e1, 1e2), param = seq(from = 0.01, to = 1, by = 0.01),
                                          x1 = 1, x2 = seq(2,10, 2))
  )


  out <- data.frame(
    kind = rep(c("time", "memory"), each = length(reps)*length(cores)*length(combs)),
    combs = rep(rep(combs, each = length(reps)*length(cores)),2),
    cores = rep(rep(cores, length(reps)*length(combs)),2),
    MC_reps = rep(rep(rep(reps, each = length(cores)), length(combs)),2),
    tidyMC = rep(rep(NA_real_, length(reps)*length(cores)*length(combs)),2),
    martin_reps = rep(rep(NA_real_, length(reps)*length(cores)*length(combs)),2),
    martin_params = rep(rep(NA_real_, length(reps)*length(cores)*length(combs)),2)
  )


  for (comb in combs){
    for(core in cores){
      for(n in reps){

        parallelisation_options <- list(seed = TRUE)

        repetitions <- n

        plan(multisession, workers = core)

        param_list <- pars[[comb]]

        param_table <- expand.grid(param_list)
        n_params <- nrow(param_table)


        speed_results <- bench::mark({
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

        out$martin_params[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "time"] <- speed_results$median[1]
        out$martin_reps[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "time"] <- speed_results$median[2]
        out$tidyMC[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "time"] <- speed_results$median[3]

        out$martin_params[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "memory"] <- speed_results$mem_alloc[1]
        out$martin_reps[out$cores == core & out$MC_reps == n & out$combs == comb & out$kind == "memory"] <- speed_results$mem_alloc[2]
        out$tidyMC[out$cores == core & out$MC_reps == n & out$combs == comb& out$kind == "memory" ] <- speed_results$mem_alloc[3]

      }
    }
  }

  return(out)

}

# martin_vs_tidyMC_plot <- martin_vs_tidyMC(reps = c(1e2, 1e3),
#                                           cores = c(1,2,3))
#
# martin_vs_tidyMC_plot_time <- martin_vs_tidyMC_plot  %>%
#   filter(kind == "time") %>%
#   pivot_longer(cols = c(tidyMC, martin_params, martin_reps)) %>%
#   ggplot(aes(x = MC_reps, y = value, col = name)) +
#   geom_line() +
#   facet_grid(cols = vars(cores),
#              rows = vars(combs)) +
#   scale_x_continuous(trans = "log10")
#
# martin_vs_tidyMC_plot_memory <- martin_vs_tidyMC_plot  %>%
#   filter(kind == "memory") %>%
#   pivot_longer(cols = c(tidyMC, martin_params, martin_reps)) %>%
#   ggplot(aes(x = MC_reps, y = value, col = name)) +
#   geom_line() +
#   facet_grid(cols = vars(cores),
#              rows = vars(combs)) +
#   scale_x_continuous(trans = "log10")

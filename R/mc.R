#' Run a Parallelized Monte Carlo Simulation
#'
#' `future_mc` runs a Monte Carlo simulation study for a user-specified function and the
#' desired parameter grids.
#'
#' @param fun The function to be evaluated. See details.
#' @param repetitions An integer that specifies the number of Monte Carlo iterations
#' @param param_list A list whose components are named after the parameters of `fun` and each
#' component is a vector containing the desired grid values for that parameter.
#' The Monte Carlo Simulation is run for all possible combinations of that parameter list.
#' @param parallelisation_plan A list whose components are named after possible parameters
#' of [future::plan()] specifying the parallelisation plan which should be used in the
#' Monte Carlo Simulation. Default is `strategy = multisession`.
#' @param parallelisation_options A list whose components are named after possible parameters
#' of [furrr::furrr_options()] for fine tuning functions, such as [furrr::future_map()]. Default is
#' `seed = TRUE` as long as not specified differently in order to assure reproducibility.
#' @param check Boolean that specifies whether a single test-iteration should be run for each parameter
#' combination in order to check for possible occuring errors in `fun`. Default is `TRUE`.
#' @param parallel Boolean that specifies whether the Monte Carlo simulation should be run in parallel.
#' Default is `TRUE`.
#' @param ... Additional parameters that are passed on to `fun` and which are not part of the parameter
#' grid.
#'
#' @details The user defined function func handles the generation of data, the
#' application of the method of interest and the evaluation of the result for a
#' single repetition and parameter combination. MonteCarlo handles the generation
#' of loops over the desired parameter grids and the repetition of the Monte Carlo
#' experiment for each of the parameter constellations.
#'
#' There are four formal requirements that `fun` has to fulfill:
#'
#' * The arguments of `fun` have to be scalar
#' * The value returned by `fun` has to be a named list
#' * The names of the returned values have to be different to the names of the arguments of `fun`.
#' Moreover, they can also not be `params`, `repetitions` or `setup`
#' * Every variable used inside `fun` has either to be defined inside `fun` or given as an argument.
#' In particular, `fun` cannot use variables which are only defined in the global environment.
#'
#' In order to use the comfort functions [plot.mc()], [summary.mc()], and [plot.summary.mc()] the
#' value returned by `fun` has to be a named list of scalars.
#'
#'
#' @return A list of type `mc` containing the following objects:
#'
#' * output: A tibble containing the return value of `fun` for each iteration and
#' parameter combination
#' * parameter: A data.frame which shows the different parameter combinations
#' * simple_output: A boolean indicating whether the return value of `fun` is a named list of
#' scalars or not
#' * nice_names: A character vector containing "nice names" for the different parameter setups
#' * calculation_time: The calculation time needed to run the whole Monte Carlo Simulation
#' * n_results: A numeric value indicating the number of results
#' * seed: The value which is used for the parameter `seed` in [furrr::furrr_options()]
#' * fun: The user-defined function `fun`
#' * repetitions: The number of repetitions run for each parameter setup
#' * parallel: Boolean whether the Monte Carlo Simulation was run in parallel or not
#' * plan: A list which was used to specify the parallelisation plan via [future::plan()]
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#'test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'data <- rnorm(n, mean = param) + x1 + x2
#'stat <- mean(data)
#'stat_2 <- var(data)
#'
#'if (x2 == 5){
#'  stop("x2 can't be 5!")
#'}
#'
#'return(list(mean = stat,sd = stat_2))
#'}
#'
#'
#'param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
#'                   x1 = 1:2, x2 = 2)
#'
#'
#'
#'
#'test <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)
future_mc <-
  function(
    fun,
    repetitions,
    param_list,
    parallelisation_plan = NULL,
    parallelisation_options = NULL,
    check = TRUE,
    parallel = TRUE,
    ...
  ){

    # Asserting inputs
    checkmate::assert_function(fun, args = names(param_list))
    checkmate::assert_int(repetitions, lower = 1)
    checkmate::assert_list(param_list, names = "named")
    # Check that all the elements in the param_list are unique
    purrr::walk(
      param_list,
      checkmate::assert_atomic_vector,
      unique = TRUE,
      .var.name = "Element of param_list"
    )
    checkmate::assert_list(parallelisation_plan, null.ok = TRUE, names = "named")
    checkmate::assert_list(parallelisation_options, null.ok = TRUE, names = "named")
    checkmate::assert_logical(check, len = 1)
    checkmate::assert_logical(parallel, len = 1)

    fun_argnames <- methods::formalArgs(fun)
    add_args <- list(...)

    checkmate::assert_list(add_args, names = "named")
    checkmate::assert_subset(names(add_args), fun_argnames)

    if(is.null(parallelisation_plan)){
      parallelisation_plan <-
        list(
          strategy = future::multisession
        )
    }

    if(is.null(parallelisation_options)){
      parallelisation_options <-
        list(
          seed = TRUE
        )
    } else {
      if(is.null(parallelisation_options$seed)) {
        parallelisation_options$seed <- TRUE
      }
    }

    if(parallel){
      do.call(future::plan, parallelisation_plan)
    }

    param_table <- expand.grid(param_list)
    param_names <- names(param_table)

    param_table_reps <-
      purrr::map_dfr(
        seq_len(repetitions),
        function(x) param_table
      )

    # Nice names for the parameters
    nice_names <-
      rep(
        purrr::map_chr(
          seq_len(nrow(param_table)),
          function(.x){
            paste(
              param_names,
              param_table[.x,],
              sep = "=",
              collapse = ", "
            )
          }),
        repetitions
      )

    if(check){

      fun_2 <- args(fun)
      body(fun_2, envir = environment()) <-
        quote({

          cl <-
            paste(
              purrr::map_chr(
                fun_argnames,
                function(.x){
                  paste(.x, get(.x), sep = " = ")
                }),
              collapse = ", "
            )

          tryCatch(
            {
              out <-
                eval(
                  parse(
                    text =
                      paste("fun(",
                            paste(
                              fun_argnames,
                              fun_argnames,
                              sep = "=",
                              collapse = ", "
                            ),
                            ")",
                            sep = ""
                      )
                  )
                )
              return(out)
            },
            error  = {
              function(e)
                paste(
                  " \n Function error: ", eval(
                    parse(
                      text =
                        paste("unlist(rlang::catch_cnd(fun(",
                              paste(
                                fun_argnames,
                                fun_argnames,
                                sep = "=",
                                collapse = ", "
                              ),
                              ")))[[1]]", sep = ""))),
                  " \n At the parameters: ",  cl, " \n" , collapse = "", sep = "")
            }
          )
        })

      # Run single test_iteration for each parameter setup
      message("Running single test-iteration for each parameter combination...")

      test_runs <-
        furrr::future_pmap(
          .l = param_table,
          .f = fun_2,
          .progress = TRUE,
          .options = do.call(furrr::furrr_options, parallelisation_options),
          ...
        )

      test_runs_errors <- unlist(test_runs) %>%
        stringr::str_subset(pattern = "^ \n Function error")

      if(length(test_runs_errors) != 0){
        stop(test_runs_errors)
      } else {
        purrr::walk(
          test_runs,
          checkmate::assert_list,
          names = "named",
          .var.name = "Return value of fun"
        )

        scalar_results <-
          purrr::map_lgl(
            test_runs,
            function(.x){
              purrr::map_lgl(
                .x, function(fun_list_outputs){
                  checkmate::test_scalar(fun_list_outputs)
                }
              ) %>%
                all()
            }
          ) %>%
          all()

        message("\n Test-run successfull: No errors occurred!")
      }

    }

    fun_2 <- args(fun)
    body(fun_2, envir = environment()) <-
      quote({
        cl <-
          paste(
            purrr::map_chr(
              fun_argnames,
              function(.x){
                paste(.x, get(.x), sep = " = ")
              }),
            collapse = ", "
          )

        tryCatch(
          {
            out <-
              eval(
                parse(
                  text =
                    paste("fun(",
                          paste(
                            fun_argnames,
                            fun_argnames,
                            sep = "=",
                            collapse = ", "
                          ),
                          ")",
                          sep = ""
                    )
                )
              )
            return(out)
          },
          error  = {
            function(e)
              stop(
                paste(
                  " \n Function error: ", eval(
                    parse(
                      text =
                        paste("unlist(rlang::catch_cnd(fun(",
                              paste(
                                fun_argnames,
                                fun_argnames,
                                sep = "=",
                                collapse = ", "
                              ),
                              ")))[[1]]", sep = ""))),
                  " \n At the parameters: ",  cl, " \n" , collapse = "", sep = "")
              )
          }
        )
      })

    # Results

    message(
      paste(
        "Running whole simulation: Overall ",
        nrow(param_table),
        " parameter combinations are simulated ...",
        sep = ""
      )
    )

    start_time <- Sys.time()

    results_list <-
      furrr::future_pmap(
        .l = param_table_reps,
        .f = fun_2,
        .progress = TRUE,
        .options = do.call(furrr::furrr_options, parallelisation_options),
        ...
      )

    # Function suggested by Martin, but is significantlly slower

    # simulator_parallelise_over_reps <-
    #   function(){
    #     furrr::future_map(
    #       .x = 1:repetitions,
    #       .f = function(.x){
    #         purrr::map_df(
    #           1:nrow(param_table),
    #           .f = function(.y) {
    #             purrr::pmap_dfr(param_table[.y,], fun, ...)
    #           }
    #         )
    #       },
    #       .progress = TRUE,
    #       .options = do.call(furrr::furrr_options, parallelisation_options)
    #     )
    #   }

    # simulator_parallelise_over_grid <- function(fun){
    #   furrr::future_map(.x = 1:repetitions, .f = function(.x){
    #     purrr::pmap(.l = param_table, .f = fun)
    #   }, .options = do.call(furrr::furrr_options,
    #                         parallelisation_options))
    # }
    #


    calculation_time <- Sys.time() - start_time
    message(paste("\n Simulation was successfull!",
                  "\n Running time: ", hms::as_hms(calculation_time),
                  collapse = ""))

    if(!check){
      scalar_results <-
        purrr::map_lgl(
          results_list[1:3],
          function(.x){
            purrr::map_lgl(
              .x, function(fun_list_outputs){
                checkmate::test_scalar(fun_list_outputs)
              }
            ) %>%
              all()
          }
        ) %>%
        all()
    }

    if(parallel){
      future::plan("default")
    }

    if(scalar_results) {

      res <-
        cbind(
          params = nice_names,
          param_table_reps,
          purrr::map_dfr(
            results_list,
            function(.x){
              .x
            }
          )
        ) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(.data$params)
    }

    if(!scalar_results){

      res <- dplyr::tibble(
        params =  nice_names,
        param_table_reps,
        results = tibble::as_tibble_col(results_list)
      )

      warning(
        "You cannot use the comfort functions: plot & summary,
        because for that fun has to return a list with named components.
        Each component has to be scalar."
      )

    }

    setups <- unique(nice_names)

    out <-
      list(
        output = res,
        parameter = param_table,
        simple_output = scalar_results,
        nice_names = setups,
        calculation_time = calculation_time,
        n_results = ncol(res) - ncol(param_table) - 1,
        seed = parallelisation_options$seed,
        fun = fun,
        repetitions = repetitions,
        parallel = parallel,
        plan = parallelisation_plan
      )

    class(out) <- "mc"

    out

  }

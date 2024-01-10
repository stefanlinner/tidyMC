#' Run a Parallelized Monte Carlo Simulation
#'
#' @description
#' `future_mc` runs a Monte Carlo simulation study
#' for a user-specified function and the
#' desired parameter grids.
#'
#' @param fun The function to be evaluated. See details.
#' @param repetitions An integer that
#' specifies the number of Monte Carlo iterations
#' @param param_list A list whose components are
#' named after the parameters of `fun` which should vary
#' for the different Monte Carlo Simulations.
#' Each component is a vector containing the desired grid
#' values for that parameter.
#' The Monte Carlo Simulation is run for all possible combinations of
#' that parameter list.
#' @param param_table Alternative to `param_list`.
#' A `data.frame` or `data.table` containing a pre-built
#' grid of values, where the columns are the parameters of `fun`
#' which should vary for the different Monte Carlo
#' Simulations.
#' This is useful if you only want to run a Monte Carlo Simulation
#' for a subset of all possible combinations.
#' @param parallelisation_plan A list whose components are named
#' after possible parameters
#' of [future::plan()] specifying the
#' parallelisation plan which should be used in the
#' Monte Carlo Simulation. Default is `strategy = multisession`.
#' @param parallelisation_options A list whose components are named
#' after possible parameters
#' of [furrr::furrr_options()] for fine tuning functions,
#' such as [furrr::future_map()]. Default is
#' `seed = TRUE` as long as not specified differently
#' in order to assure reproducibility.
#' @param check Boolean that specifies whether a single test-iteration
#' should be run for each parameter
#' combination in order to check for possible
#' occuring errors in `fun`. Default is `TRUE`.
#' @param parallel Boolean that specifies whether
#'  the Monte Carlo simulation should be run in parallel.
#' Default is `TRUE`.
#' @param ... Additional parameters that are passed on to `fun`
#' and which are not part of the parameter
#' grid.
#'
#' @details The user defined function `fun` handles
#' (if specified) the generation of data, the
#' application of the method of interest and the evaluation of the result for a
#' single repetition and parameter combination.
#' `future_mc` handles the generation of loops over the desired parameter grids
#' and the repetition of the Monte Carlo experiment
#' for each of the parameter constellations.
#'
#' There are four formal requirements that `fun` has to fulfill:
#'
#' * The arguments of `fun` which are present in `param_list`
#' need to be scalar values.
#' * The value returned by `fun` has to be a named list
#' and must have the same components for each
#' iteration and parameter combination.
#' * The names of the returned values and
#' those of the arguments contained in `param_list` need to
#' be different.
#' Moreover, they cannot be `"params"`, `"repetitions"` or `"setup"`
#' * Every variable used inside `fun` has either to be defined inside `fun`
#' or given as an argument through the `...` argument.
#' In particular, `fun` cannot use variables which are only defined
#' in the global environment.
#'
#' In order to use the comfort functions
#' [plot.mc()], [summary.mc()], [plot.summary.mc()], and
#' [tidy_mc_latex()] the value returned by `fun`
#' has to be a named list of scalars.
#'
#'
#' @return A list of type `mc` containing the following objects:
#'
#' * output: A tibble containing the return value of `fun`
#' for each iteration and
#' parameter combination
#' * parameter: A tibble which shows the different parameter combinations
#' * simple_output: A boolean value indicating
#' whether the return value of `fun` is a named list of
#' scalars or not
#' * nice_names: A character vector containing "nice names"
#' for the different parameter setups
#' * calculation_time: The calculation time needed
#' to run the whole Monte Carlo Simulation
#' * n_results: A numeric value indicating the number of results
#' * seed: The value which is used for
#' the parameter `seed` in [furrr::furrr_options()]
#' * fun: The user-defined function `fun`
#' * repetitions: The number of repetitions run for each parameter setup
#' * parallel: Boolean whether the Monte Carlo Simulation
#' was run in parallel or not
#' * plan: A list that specified the parallelisation plan via [future::plan()]
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2) {
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5) {
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(
#'   param = seq(from = 0, to = 1, by = 0.5),
#'   x1 = 1:2
#' )
#'
#' set.seed(101)
#' test_mc <- future_mc(
#'   fun = test_func,
#'   repetitions = 1000,
#'   param_list = param_list,
#'   n = 10,
#'   x2 = 2
#' )
#'
future_mc <-
  function(
    fun,
    repetitions,
    param_list = NULL,
    param_table = NULL,
    parallelisation_plan = list(strategy = future::multisession),
    parallelisation_options = list(),
    check = TRUE,
    parallel = TRUE,
    ...
  ) {
    checkmate::assert_function(fun, args = names(param_list))
    checkmate::assert_int(repetitions, lower = 1)
    checkmate::assert_list(param_list, names = "named", null.ok = TRUE)
    if (!is.null(param_list)) {
      purrr::walk(
        param_list,
        checkmate::assert_atomic_vector,
        unique = TRUE,
        .var.name = "Element of param_list"
      )
    }
    checkmate::assert_data_frame(param_table,
                                 col.names = "named",
                                 null.ok = TRUE
    )
    if (!is.null(param_list) & !is.null(param_table)) {
      stop("Only one of the arguments param_list and param_table
           can be defined at the same time!")
    }
    if (is.null(param_list) & is.null(param_table)) {
      stop("At least one of the arguments param_list and param_table
           has to be defined!")
    }
    checkmate::assert_list(parallelisation_plan,
                           null.ok = TRUE,
                           names = "named"
    )
    checkmate::assert_list(parallelisation_options,
                           null.ok = TRUE,
                           names = "named"
    )
    checkmate::assert_logical(check, len = 1)
    checkmate::assert_logical(parallel, len = 1)

    fun_argnames <- methods::formalArgs(fun)
    add_args <- list(...)

    checkmate::assert_subset(names(param_list),
                             choices = fun_argnames,
                             empty.ok = TRUE)
    checkmate::assert_subset(names(param_table),
                             choices = fun_argnames,
                             empty.ok = TRUE)
    checkmate::assert_list(add_args, names = "named")
    checkmate::assert_subset(names(add_args), fun_argnames)

    parallelisation_options_default <- list(seed = TRUE)

    parallelisation_options <-
      utils::modifyList(parallelisation_options_default,
                        parallelisation_options)

    if (!parallel) {
      parallelisation_plan <-
        list(
          strategy = future::sequential
        )
    }

    do.call(future::plan, parallelisation_plan)

    if (!is.null(param_list)) {
      param_table <- expand.grid(param_list)
    }
    param_names <- names(param_table)

    scalar_results <- NULL

    if (check) {
      deparsed_function <- deparse(fun)
      fun_2 <- eval(
        parse(
          text =
            stringr::str_c(
              stringr::str_c(deparsed_function[1:2], collapse = ""),
              "\n",
              "cl <-
                  stringr::str_c(
                    param_names,
                    eval(parse(text  =
                              stringr::str_c(\"c(\",
                                  stringr::str_c(param_names,
                                  sep = \"\", collapse = \", \"),
                                  \")\",
                               sep = \"\", collapse = \"\")
                               )
                        ),
                  sep = \"=\", collapse = \", \")",
              "\n",
              stringr::str_c(
                "tryCatch({fun(",
                stringr::str_c(fun_argnames,
                               fun_argnames,
                               sep = "=",
                               collapse = ", "),
                ")",
                sep = "", collapse = ""
              ),
              "}, ",
              "\n",
              "error  ={ ",
              stringr::str_c(
                "function(e) stringr::str_c(\" \n Function error: \",
                unlist(rlang::catch_cnd(fun(",
                stringr::str_c(fun_argnames,
                               fun_argnames,
                               sep = "=",
                               collapse = ", "),
                ")))[[1]], \" \n At the parameters: \",
                cl, \" \n \", collapse = \"\", sep = \"\")",
                sep = "", collapse = ""
              ),
              "});",
              "\n",
              "}",
              sep = "", collapse = ""
            )
        )
      )

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

      if (length(test_runs_errors) != 0) {
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
            function(.x) {
              purrr::map_lgl(
                .x, function(fun_list_outputs) {
                  checkmate::test_scalar(fun_list_outputs,
                                         na.ok = TRUE,
                                         null.ok = TRUE)
                }
              ) %>%
                all()
            }
          ) %>%
          all()

        message("\n Test-run successfull: No errors occurred!")
      }
    }

    message(
      stringr::str_c(
        "Running whole simulation: Overall ",
        nrow(param_table),
        " parameter combinations are simulated ...",
        sep = ""
      )
    )

    output_generator <- function(x) {
      if (is.null(scalar_results)) {
        scalar_results <<-
          purrr::map_lgl(
            x[seq_len(nrow(param_table))],
            function(.x) {
              purrr::map_lgl(
                .x, function(fun_list_outputs) {
                  checkmate::test_scalar(fun_list_outputs,
                                         na.ok = TRUE,
                                         null.ok = TRUE)
                }
              ) %>%
                all()
            }
          ) %>%
          all()
      }

      if (scalar_results) {
        return(purrr::map_dfr(.x = x, function(.x) .x) %>%
                 dplyr::as_tibble())
      }

      if (!scalar_results) {
        warning(
          "You cannot use the comfort functions: plot & summary,
        because for that fun has to return a list with named components.
        Each component has to be scalar."
        )
        return(tibble::as_tibble_col(x))
      }
    }

    . <- NULL

    if (parallel) {
      start_time <- Sys.time()

      res_table_reps <-
        purrr::map_dfr(
          seq_len(repetitions),
          function(x) param_table
        ) %>%
        data.frame(
          params = rep(
            purrr::map_chr(
              seq_len(nrow(param_table)),
              function(.x) {
                stringr::str_c(
                  param_names,
                  param_table[.x, ],
                  sep = "=",
                  collapse = ", "
                )
              }
            ),
            repetitions
          ),
          .,
          furrr::future_pmap(
            .l = .,
            .f = fun,
            .progress = TRUE,
            .options = do.call(furrr::furrr_options, parallelisation_options),
            ...
          ) %>%
            output_generator()
        ) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(.data$params)

      calculation_time <- Sys.time() - start_time
    }

    if (!parallel) {
      start_time <- Sys.time()

      res_table_reps <-
        purrr::map_dfr(
          seq_len(repetitions),
          function(x) param_table
        ) %>%
        data.frame(
          params = rep(
            purrr::map_chr(
              seq_len(nrow(param_table)),
              function(.x) {
                stringr::str_c(
                  param_names,
                  param_table[.x, ],
                  sep = "=",
                  collapse = ", "
                )
              }
            ),
            repetitions
          ),
          .,
          purrr::pmap(
            .l = .,
            .f = fun,
            ...
          ) %>%
            output_generator()
        ) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(.data$params)

      calculation_time <- Sys.time() - start_time
    }

    message(stringr::str_c("\n Simulation was successfull!",
                           "\n Running time: ", hms::as_hms(calculation_time),
                           collapse = "", sep = ""
    ))

    future::plan("default")

    out <-
      list(
        output = res_table_reps,
        parameter = param_table %>%
          dplyr::as_tibble(),
        simple_output = scalar_results,
        nice_names = unique(res_table_reps$params),
        calculation_time = calculation_time,
        n_results = ncol(res_table_reps) - ncol(param_table) - 1,
        seed = parallelisation_options$seed,
        fun = fun,
        repetitions = repetitions,
        parallel = parallel,
        plan = parallelisation_plan
      )

    class(out) <- "mc"

    out
  }

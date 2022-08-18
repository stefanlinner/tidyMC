#' Test title
#'
#' test description
#'
#' @param repetitions test
#' @param param_list test
#' @param packages test
#' @param parallelisation_plan test
#' @param parallelisation_options test
#' @param ... test
#' @param fun tes
#' @param check tes
#' @param parallel tes
#'
#' @return returns simulation results
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang search_envs
#'
#' @examples
#'
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
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
#'
future_mc <-
  function(
    fun,
    repetitions,
    param_list,
    packages = NULL,
    parallelisation_plan = NULL,
    parallelisation_options = NULL,
    check = TRUE,
    parallel = TRUE,
    # parallelise_over = NULL,
    ...
  ){

    # Asserting inputs
    checkmate::assert_function(fun, args = names(param_list))
    checkmate::assert_int(repetitions, lower = 1)
    checkmate::assert_list(param_list, names = "named")
    checkmate::assert_character(packages, null.ok = TRUE)
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

    # Packages extraction
    if (is.null(packages)){
      pckgs <- rlang::search_envs() %>% names()
      pckgs <- pckgs[stringr::str_detect(pckgs, pattern = "package:")] %>%
        sub(pattern = "package:", replacement = "")
      pckgs <- pckgs[!(pckgs %in% c("base", "methods", "datasets"))]
      parallelisation_options <- append(parallelisation_options, list(packages = pckgs))
    } else {
      parallelisation_options <- append(parallelisation_options, list(packages = packages))
    }




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

      ## HUHU: Insert if condition to skip test --> Then use old simulation (fail early)

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
          names = "named"
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

        message("Test-run successfull: No errors occurred!")
      }

    }

    if(!check){

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
    }

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


    out <-
      list(
        output = res,
        parameter = param_table,
        simple_output = scalar_results
      )

    class(out) <- "mc"

    out

  }

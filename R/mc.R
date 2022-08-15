#' Test title
#'
#' test description
#'
#' @param func test
#' @param repetitions test
#' @param param_list test
#' @param packages test
#' @param parallelisation_plan test
#' @param parallelisation_options test
#' @param ... test
#'
#' @return returns simulation results
#' @export
#'
#' @importFrom magrittr %>%
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
#'test <- future_mc(func = test_func, repetitions = 1000, param_list = param_list)
#'
future_mc <-
  function(
    func,
    repetitions,
    param_list,
    packages = "test",
    parallelisation_plan = NULL,
    parallelisation_options = NULL,
    ...
    # , simple = FALSE
  ){

    # Asserting inputs
    checkmate::assert_function(func, args = names(param_list))
    checkmate::assert_int(repetitions, lower = 1)
    checkmate::assert_list(param_list)
    purrr::walk(
      seq_along(param_list),
      checkmate::assert_vector
    )
    checkmate::assert_character(packages)
    checkmate::assert_list(parallelisation_plan, null.ok = TRUE, names = "named")
    checkmate::assert_list(parallelisation_options, null.ok = TRUE, names = "named")

    # checkmate::assert_logical(simple, len = 1)

    func_argnames <- methods::formalArgs(func)

    add_args <- list(...)
    checkmate::assert_list(add_args)
    checkmate::assert_subset(names(add_args), func_argnames)

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

    do.call(future::plan, parallelisation_plan)

    aux <- dplyr::as_tibble(
      expand.grid(param_list)
    )

    # Aux but for the number of repetitions could be improved
    aux2 <-
      purrr::map_dfr(
        seq_len(repetitions),
        function(x) aux
      )

    # Nice names for the parameters
    nice_names <-
      rep(
        purrr::map_chr(
          seq_len(nrow(aux)),
          function(.x){
            paste(
              purrr::map_chr(
                names(aux),
                function(.z){
                  paste(
                    .z,
                    "=",
                    aux2[.x, .z],
                    sep = ""
                  )
                }
              ),
              collapse = ", "
            )
          }),
        repetitions
      )


    # New function based on func that gives us the error message with the
    # function call

    func_2 <- args(func)
    body(func_2, envir = environment()) <-
      quote({

        cl <-
          paste(
            purrr::map_chr(
              func_argnames,
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
                    paste("func(",
                          paste(
                            func_argnames,
                            func_argnames,
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
              #stop( # I don't want early exit -> We prefer test-run (alternatively if test = F,
              # then with stop!)
              paste(
                " \n Function error: ", eval(
                  parse(
                    text =
                      paste("unlist(rlang::catch_cnd(func(",
                            paste(
                              func_argnames,
                              func_argnames,
                              sep = "=",
                              collapse = ", "
                            ),
                            ")))[[1]]", sep = ""))),
                " \n At the parameters: ",  cl, " \n" , collapse = "", sep = "")
            #)
          }
        )
      })

    # Run single test_iteration for each parameter setup

    message("Running single test-iteration for each parameter combination...")

    test_runs <-
      furrr::future_pmap(
        .l = aux,
        .f = func_2,
        .progress = TRUE,
        .options = do.call(furrr::furrr_options, parallelisation_options),
        ...
      )

    test_runs_errors <- unlist(test_runs) %>%
      stringr::str_subset(pattern = "^ \n Function error")


    if(length(test_runs_errors != 0)){
      stop(test_runs_errors)
    } else {
      purrr::walk(
        test_runs,
        checkmate::assert_list,
        names = "named"
      )
      message("Test-run successfull: No errors occurred!")
    }

    # Results

    message("Running whole simulation...")

    results_list <-
      furrr::future_pmap(
        .l = aux2,
        .f = func_2,
        .progress = TRUE,
        .options = do.call(furrr::furrr_options, parallelisation_options),
        ...
      )

    message("\n Simulation was successfull!")

    future::plan("default")

    res <- dplyr::tibble(
      params =  nice_names,
      results = tibble::as_tibble_col(results_list)
    )

    aux$setup <- unique(nice_names)

    out <- list(output = res, sim_setups = aux)

    class(out) <- "mc"

    out

  }

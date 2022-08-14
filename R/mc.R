#' Test title
#'
#' test description
#'
#' @param func test
#' @param repetitions test
#' @param param_list test
#' @param packages test
#' @param parallelisation_plan test
#' @param simple test
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
#'return(list(stat, stat_2))
#'}
#'
#'
#'param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
#'                   x1 = 1:2, x2 = 2)
#'
#'
#'
#'
#'test <- future_mc(func = test_func, repetitions = 1000, param_list = param_list, simple = TRUE)
#'
future_mc <-
  function(
    func,
    repetitions,
    param_list,
    packages = "test",
    parallelisation_plan = NULL,
    simple = FALSE
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
    checkmate::assert_list(parallelisation_plan, null.ok = TRUE)
    checkmate::assert_logical(simple, len = 1)

    if(is.null(parallelisation_plan)){
      parallelisation_plan <-
        list(
          strategy = future::multisession,
          substitute = TRUE,
          .skip = FALSE,
          .call = TRUE,
          .cleanup = TRUE,
          .init = TRUE
        )
    }

    do.call(future::plan, parallelisation_plan)

    # Order parameter names to match function order
    func_argnames <- methods::formalArgs(func)
    param_names <- names(param_list)
    param_list <- param_list[order(match(func_argnames, param_names))]
    param_names <- names(param_list)

    n_param <- length(param_list)
    aux <- dplyr::as_tibble(
      expand.grid(param_list)
    )
    grid_size <- nrow(aux)

    # Aux but for the number of repetitions could be improved / future needed?
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
        repetitions)


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
        .options = furrr::furrr_options(seed = TRUE)
      )

    test_runs_errors <- unlist(test_runs) %>%
      stringr::str_subset(pattern = "^ \n Function error")

    if(length(test_runs_errors != 0)){
      stop(test_runs_errors)
    } else {
      message("Test-run successfull: No errors occurred!")
    }

    # Check the number of results
    num_res <- length(unlist(test_runs[[1]]))


    # Results

    message("Running whole simulation...")

    results_list <-
      furrr::future_pmap(
        .l = aux2,
        .f = func_2,
        .progress = TRUE,
        .options = furrr::furrr_options(seed = TRUE)
      )

    message("Simulation was successfull!")

    # Extract results

    if(simple){

      res_names <-
        purrr::map_chr(
          seq_len(num_res),
          function(.x){
            paste("res_", .x, sep = "")
          }
        )

      out <-
        purrr::map_dfr(
          results_list,
          function(.x){
            res <- .x
            names(res) <- res_names
            res
          }
        )

      out <- dplyr::as_tibble(cbind(param = nice_names, out))

    } else {
      out <- dplyr::tibble(
        params =  nice_names,
        results = tibble::as_tibble_col(results_list)
      )
    }

    future::plan("default")

    out
  }

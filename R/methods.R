#' Result summaries of a mc object
#'
#' @param object test
#' @param ... test
#' @param sum_funs test
#'
#' @return result summaries
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#' data <- rnorm(n, mean = param) + x1 + x2
#' stat <- mean(data)
#' stat_2 <- var(data)
#'
#' if (x2 == 5){
#'   stop("x2 can't be 5!")
#' }
#'
#' return(list(mean = stat, sd = stat_2))
#' }
#'
#'
#' param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2, x2 = 2)
#'
#'
#'
#'
#' test <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)
#' summary(test)
summary.mc <- function(object, sum_funs = NULL, ...){

  checkmate::assert_class(object, "mc")
  if(!object$simple_output){
    stop("fun has to return a list with named components. Each component has to be scalar.")
  }
  param_names <- names(object$parameter)
  stat_names <- dplyr::setdiff(names(object$output), c("params", param_names))
  setup_names <- unique(object$output$params)
  checkmate::assert_list(sum_funs, null.ok = TRUE)
  if(!is.null(sum_funs)){
    checkmate::assert_choice(
      length(sum_funs), c(length(stat_names), length(setup_names))
    )
    purrr::walk(
      sum_funs,
      function(.x){
        checkmate::assert(
          {checkmate::check_list(.x ,names = "named")},
          {checkmate::check_function(.x)},
          combine = "or"
        )
      }
    )
  }

  if(is.null(sum_funs)){

    sum_out <-
      object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{
        purrr::map(
          .x[, stat_names],
          function(.y){

            if(is.numeric(.y)){

              out <- list(
                mean = mean(.y),
                mean_over_reps = cumsum(.y) / seq_along(.y)
              )
              return(out)
            }

            if(!is.numeric(.y)){
              return(summary(.y))
            }
          }
        )
      }) %>%
      purrr::set_names(setup_names)

  }



  if(!is.null(sum_funs) & length(sum_funs) == length(stat_names) & is.function(sum_funs[[1]])){

    checkmate::assert_list(sum_funs, names = "named")
    checkmate::assertNames(
      names(sum_funs),
      permutation.of = stat_names
    )
    purrr::walk(
      sum_funs,
      checkmate::assert_function
    )

    sum_out <-
      object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{

        purrr::map(
          stat_names,
          function(.y){

            sum_func_out <- sum_funs[[.y]](.[[.y]])
            if(checkmate::test_number(sum_func_out)){

              sum_func_over_reps <-
                purrr::map_dbl(
                  seq_along(.[[.y]]),
                  function(.z) {
                    sum_funs[[.y]](.[[.y]][1:.z])
                  }
                )

              return(
                list(
                  sum_func = sum_func_out,
                  sum_func_over_reps = sum_func_over_reps
                )
              )

            } else {
              return(sum_func_out)
            }

          }
        ) %>%
          purrr::set_names(stat_names)

      }) %>%
      purrr::set_names(setup_names)

  }


  if(!is.null(sum_funs) & length(sum_funs) == length(setup_names) & is.list(sum_funs[[1]])){

    checkmate::assert_list(sum_funs, names = "named")
    checkmate::assertNames(
      names(sum_funs),
      permutation.of = setup_names
    )

    purrr::walk(
      sum_funs,
      function(.x){
        checkmate::assert_list(
          .x,
          names = "named",
          len = length(stat_names)
        )
        checkmate::assertNames(
          names(.x),
          permutation.of = stat_names
        )
        purrr::walk(
          .x,
          function(.y){
            checkmate::assert_function
          }
        )
      }
    )

    sum_out <-
      object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{

        setup <- unique(.$params)

        purrr::map(
          stat_names,
          function(.y){


            sum_func_out <- sum_funs[[setup]][[.y]](.[[.y]])

            if(checkmate::test_number(sum_func_out)){

              sum_func_over_reps <-
                purrr::map_dbl(
                  seq_along(.[[.y]]),
                  function(.z) {
                    sum_funs[[setup]][[.y]](.[[.y]][1:.z])
                  }
                )

              return(
                list(
                  sum_func = sum_func_out,
                  sum_func_over_reps = sum_func_over_reps
                )
              )

            } else {
              return(sum_func_out)
            }
          }
        ) %>%
          purrr::set_names(stat_names)

      }, .keep = TRUE) %>%
      purrr::set_names(setup_names)


  }

  class(sum_out) <- "summary.mc"

  sum_out

}


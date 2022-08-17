#' Result summaries of a mc object
#'
#' @param object test
#' @param ... test
#' @param sum_funcs test
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
#' test <- future_mc(func = test_func, repetitions = 1000, param_list = param_list)
#' summary(test)
summary.mc <- function(object, sum_funcs = NULL, ...){

  checkmate::assert_class(object, "mc")
  if(!object$simple_output){
    stop("fun has to return a list with named components. Each component has to be scalar.")
  }
  stat_names <- dplyr::setdiff(names(object$output), "params")
  setup_names <- object$setups
  checkmate::assert_list(sum_funcs, null.ok = TRUE)
  if(!is.null(sum_funcs)){
    checkmate::assert_choice(
      length(sum_funcs), c(length(stat_names), length(setup_names))
    )
    purrr::walk(
      sum_funcs,
      function(.x){
        checkmate::assert(
          {checkmate::check_list(.x ,names = "named")},
          {checkmate::check_function(.x)},
          combine = "or"
        )
      }
    )
  }

  if(is.null(sum_funcs)){

    sum_out <-
      object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{
        purrr::map(
          .x,
          function(.y){
            summary(.y)
          }
        )
      }) %>%
      purrr::set_names(setup_names)

    return(sum_out)

  }



  if(!is.null(sum_funcs) & length(sum_funcs) == length(stat_names) & is.function(sum_funcs[[1]])){

    checkmate::assert_list(sum_funcs, names = "named")
    checkmate::assertNames(
      names(sum_funcs),
      permutation.of = stat_names
    )
    purrr::walk(
      sum_funcs,
      checkmate::assert_function
    )

    sum_out <-
      object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{

        stat_names <- names(.)
        purrr::map(
          stat_names,
          function(.y){
            sum_funcs[[.y]](.[[.y]])
          }
        ) %>%
          purrr::set_names(stat_names)

      }) %>%
      purrr::set_names(setup_names)

    return(sum_out)

  }


  if(!is.null(sum_funcs) & length(sum_funcs) == length(setup_names) & is.list(sum_funcs[[1]])){

    checkmate::assert_list(sum_funcs, names = "named")
    checkmate::assertNames(
      names(sum_funcs),
      permutation.of = setup_names
    )

    purrr::walk(
      sum_funcs,
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
            sum_funcs[[setup]][[.y]](.[[.y]])
          }
        ) %>%
          purrr::set_names(stat_names)

      }, .keep = TRUE) %>%
      purrr::set_names(setup_names)

    return(sum_out)

  }
}


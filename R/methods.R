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


#' Plotting mc object
#'
#' @param join testt
#' @param ... test
#' @param x test
#' @param which test
#'
#' @return returns a plot
#' @export
#'
#' @examples
#'
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
#' plot(test)
plot.mc <- function(x, join = NULL, which = NULL, ...){

  checkmate::assert_class(x, "mc")
  if(!x$simple_output){
    stop("fun has to return a list with named components. Each component has to be scalar.")
  }
  param_names <- names(x$parameter)
  stat_names <- dplyr::setdiff(names(x$output), c("params", param_names))
  setup_names <- unique(x$output$params)
  checkmate::assert_subset(join, setup_names, empty.ok = TRUE)
  checkmate::assert_subset(which, setup_names, empty.ok = TRUE)

  data_plot <-
    x$output

  if(!is.null(which)) {
    data_plot <-
      data_plot %>%
      dplyr::filter(.data$params %in% which)
  }

  if(!is.null(join)){
    data_plot <-
      data_plot %>%
      dplyr::filter(.data$params %in% join)
  }

  if(is.null(join)){

    plots_which <-
      purrr::map(
        stat_names,
        function(stat){
          if(is.numeric(data_plot[[stat]])){
            plot_stat <-
              data_plot %>%
              ggplot2::ggplot(ggplot2::aes_string(stat)) +
              ggplot2::geom_density() +
              ggplot2::facet_grid(~.data$params) +
              ggplot2::theme_bw()
            print(plot_stat)
            plot_stat
          } else {
            plot_stat <-
              data_plot %>%
              ggplot2::ggplot(ggplot2::aes_string(stat)) +
              ggplot2::geom_bar() +
              ggplot2::facet_grid(~.data$params) +
              ggplot2::theme_bw()
            print(plot_stat)
            plot_stat
          }
        }
      ) %>%
      purrr::set_names(stat_names)

    return(invisible(plots_which))

  }

  if(!is.null(join)) {

    plots_joint <-
      purrr::map(
        stat_names,
        function(stat){
          if(is.numeric(data_plot[[stat]])){
            plot_stat <-
              data_plot %>%
              ggplot2::ggplot(ggplot2::aes_string(stat, col = "params")) +
              ggplot2::geom_density() +
              ggplot2::theme_bw() +
              ggplot2::labs(title = paste(
                "Joint density plot of", length(join), "setups for the statistic", stat,sep = " "
              ), color = "Setups") +
              ggplot2::theme(legend.position = "bottom")

            print(plot_stat)
            plot_stat

          }
        }
      ) %>%
      purrr::set_names(stat_names)

    plots_joint <- plots_joint[!purrr::map_lgl(plots_joint, is.null)]

    return(invisible(plots_joint))

  }

}



#' Plot of summary of mc object
#'
#' @param x test
#' @param ... tse
#'
#' @return tet
#' @export
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
#' plot(summary(test))
plot.summary.mc <- function(x, ...) {

  checkmate::assert_class(x, "summary.mc")
  setup_names <- names(x)
  stat_names <- names(x[[1]])

  data_plot <-
    purrr::map(
      stat_names,
      function(stat){
        purrr::map_dfc(
          setup_names,
          function(setup){
            if(checkmate::test_number(x[[setup]][[stat]][[1]])){
              stat_dat <- list(x[[setup]][[stat]][[2]])
              names(stat_dat) <- setup
              return(stat_dat)
            } else {
              stat_dat <- list(NA)
              names(stat_dat) <- setup
              return(stat_dat)
            }
          }
        ) %>%
          tibble::rowid_to_column(var = "Replications") %>%
          tidyr::pivot_longer(cols = setup_names, names_to = "setup", values_to = stat)
      }
    ) %>%
    purrr::set_names(stat_names)

  data_plot <- data_plot[purrr::map_lgl(
    data_plot,
    function(data){
      !all(is.na(data[[3]]))
    }
  )]

  plots_over_reps <-
    purrr::map(
      stat_names,
      function(stat){
        if(!is.null(data_plot[[stat]])){
          plot_stat <-
            data_plot[[stat]] %>%
            ggplot2::ggplot(ggplot2::aes_string(x = "Replications", y = stat)) +
            ggplot2::geom_line() +
            ggplot2::facet_grid(~.data$setup) +
            ggplot2::theme_bw()
          print(plot_stat)
          plot_stat
        }
      }
    )

  return(invisible(plots_over_reps))

}

#' Print method for mc object
#'
#' @param object test
#'
#' @return print output
#' @export
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
#' print(test)

print.mc <- function(object){
  cat("Monte Carlo simulation results.",
      "\n", "Using:", length(object$setups), "parameter combinations,",
      "\n", object$repetitions, "repetitions and",
      "\n", "Seed:", object$seed,
      "\n", "Specified function:", "\n", paste(deparse(object$test_function),
                                               collapse = "\n"))

}





























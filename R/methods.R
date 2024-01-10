#' Summarize the Results of a Monte Carlo Simulation
#'
#' @description
#' Summarize the results of a Monte Carlo Simulation run by [future_mc()] with
#' (optionally) user-defined summary functions.
#'
#' @param object An object of class `mc`,
#' for which holds `simple_output = TRUE`.
#' See value of [future_mc()].
#' @param sum_funs A named (nested) list containing summary functions.
#' See details.
#' @param which_path A character vector containing the names of (some of)
#' the named outputs
#' (the names of the returned list of `fun` in [future_mc()]),
#' for which to return a "path" of the
#' stepwise calculation of the result of the summary function.
#' Alternatively, `"all"` or `"none"` can be used to return either
#' the path for all or none of the
#' numeric outputs.
#' Default: `"all"`.
#' @param ... Ignored
#'
#' @details In order to use `summary()`,
#' the output of [future_mc()] has to be "simple",
#' which is the case if the return value of `fun` is a named list of scalars.
#' If the
#' returned value of `fun` is a named list of more complex data structures,
#' `summary()`
#' cannot be used.
#'
#' With `sum_funs` the user can define (different) functions which summarize
#' the simulation results for each output
#' (return values of `fun` in [future_mc()])
#' and each parameter combination.
#' Thus, the functions inside `sum_funs` only take one argument,
#' which is the output vector (with length `repetitions`) of one output
#' of one specific parameter combination.
#'
#' The default summary functions are [base::mean()] for numeric outputs and
#' [base::summary()] for outputs with non-numeric data types.
#'
#' The user can define summary functions by supplying a named
#' (nested) list to `sum_funs`. When
#' the functions provided for each output return only one numeric value
#' the results are twofold:
#' first, a single scalar result of the
#' function evaluating the whole output vector.
#' Second, a "path" with length `repetitions` of the
#' stepwise calculation of the function's result
#' across the output vector
#' (assumed that the output is contained in `which_path`).
#'
#' If the user wants to summarize the simulation results of a respective output
#' in the same way
#' for each parameter combination, a list whose components are named after the
#' outputs (the names of the returned
#' list of `fun` in [future_mc()]) is supplied and each component is
#' a function which only takes the vector of results
#' of one output as the main argument.
#'
#' If the user wants to summarize the simulation
#' results of a respective output differently for
#' different parameter combinations, a nested list has to be supplied.
#' The components of the outer list
#' must be equal in length and naming to the `nice_names` of the parameter
#' combinations (see value of [future_mc()]) and each component is another
#' list (inner list). The components of the inner list are then defined the
#' same way as above
#' (components named after the outputs and each component is a function).
#'
#' The provided summary functions are not restricted regarding the complexity
#' of their return value.
#' However, the path of the summarized output over all simulation repetitions
#' is only returned if the
#' provided summary functions return a single numeric value
#' (and the output is contained in `which_path`).
#' Thus, [plot.summary.mc()] will only work in this specific case.
#'
#'
#' @return A list of type `summary.mc` containing the
#' result of the summary functions of the simulation
#' results of a respective output and parameter combination.
#'
#' If the provided summary functions return a single numeric value,
#' the path of the summarized output
#' (which are contained in `which_path`)
#' over all simulation repetitions is also returned.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2)
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
#' summary(test_mc)
#' summary(test_mc, sum_funs = list(mean = mean, var = sd))
#'
#' sum_funcs <- list(
#'   list(
#'     mean = mean, var = sd
#'   ),
#'   list(
#'     mean = mean, var = summary
#'   ),
#'   list(
#'     mean = max, var = min
#'   ),
#'   list(
#'     mean = mean, var = sd
#'   ),
#'   list(
#'     mean = mean, var = summary
#'   ),
#'   list(
#'     mean = max, var = min
#'   )
#' )
#'
#' names(sum_funcs) <- test_mc$nice_names
#'
#' summary(test_mc, sum_funs = sum_funcs)
#'
summary.mc <-
  function(
    object,
    sum_funs = NULL,
    which_path = "all",
    ...
  ){

    checkmate::assert_class(object, "mc")
    if(!object$simple_output){
      stop("fun has to return a list with named components.
         Each component has to be scalar.")
    }
    param_names <- names(object$parameter)
    stat_names <- dplyr::setdiff(names(object$output), c("params", param_names))
    setup_names <- unique(object$output$params)

    if("all" %in% which_path){
      checkmate::assert_string(which_path, pattern = "^all$")
    } else if("none" %in% which_path){
      checkmate::assert_string(which_path, pattern = "^none$")
    } else {
      checkmate::assert_subset(which_path, stat_names, empty.ok = FALSE)
    }

    if("all" %in% which_path){
      stat_names_path <- stat_names
    } else if("none" %in% which_path){
      stat_names_path <- NULL
    } else {
      stat_names_path <- which_path
    }

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
            stat_names,
            function(stat){

              if(is.numeric(.x[[stat]])){

                mean_out <- mean(.x[[stat]])

                if(stat %in% stat_names_path & !all(is.na(.x[[stat]]))){
                  mean_over_reps <-
                    {cumsum(.x[[stat]]) / seq_along(.x[[stat]])} %>%
                    unname()
                }

                if(exists("mean_over_reps")){
                  out <- list(
                    mean = mean_out,
                    mean_over_reps = mean_over_reps
                  )
                } else {
                  out <- list(
                    mean = mean_out
                  )
                }

                return(out)
              }

              if(!is.numeric(.x[[stat]])){
                return(list(summary = summary(.x[[stat]])))
              }
            }
          )%>%
            purrr::set_names(stat_names)
        }) %>%
        purrr::set_names(setup_names)

    }



    if(!is.null(sum_funs) &
       length(sum_funs) == length(stat_names) &
       is.function(sum_funs[[1]])){

      checkmate::assert_list(sum_funs, names = "named")
      checkmate::assertNames(
        names(sum_funs),
        permutation.of = stat_names
      )
      purrr::walk(
        sum_funs,
        checkmate::assert_function,
        .var.name = "sum_funs"
      )

      sum_out <-
        object$output %>%
        dplyr::group_by(.data$params) %>%
        dplyr::group_map(~{

          purrr::map(
            stat_names,
            function(.y){

              sum_func_out <- sum_funs[[.y]](.[[.y]])
              if(checkmate::test_number(sum_func_out) & .y %in% stat_names_path){

                sum_func_over_reps <-
                  purrr::map_dbl(
                    seq_along(.[[.y]]),
                    function(.z) {
                      sum_funs[[.y]](.[[.y]][1:.z])
                    }
                  ) %>%
                  unname()

                return(
                  list(
                    sum_func = sum_func_out,
                    sum_func_over_reps = sum_func_over_reps
                  )
                )

              } else {
                return(list(sum_func = sum_func_out))
              }

            }
          ) %>%
            purrr::set_names(stat_names)

        }) %>%
        purrr::set_names(setup_names)

    }


    if(!is.null(sum_funs) &
       length(sum_funs) == length(setup_names) &
       is.list(sum_funs[[1]])){

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
            len = length(stat_names),
            .var.name = "sum_funs"
          )
          checkmate::assertNames(
            names(.x),
            permutation.of = stat_names,
            .var.name = "sum_funs"
          )
          purrr::walk(
            .x,
            function(.y){
              checkmate::assert_function(.y,
                                         .var.name = "sum_funs")
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

              if(checkmate::test_number(sum_func_out) & .y %in% stat_names_path){

                sum_func_over_reps <-
                  purrr::map_dbl(
                    seq_along(.[[.y]]),
                    function(.z) {
                      sum_funs[[setup]][[.y]](.[[.y]][1:.z])
                    }
                  ) %>%
                  unname()

                return(
                  list(
                    sum_func = sum_func_out,
                    sum_func_over_reps = sum_func_over_reps
                  )
                )

              } else {
                return(list(sum_func = sum_func_out))
              }
            }
          ) %>%
            purrr::set_names(stat_names)

        }, .keep = TRUE) %>%
        purrr::set_names(setup_names)


    }

    class(sum_out) <- "summary.mc"

    attributes(sum_out)$n_reps <- object$repetitions

    sum_out

  }


#' Plot the results of a Monte Carlo Simulation
#'
#' @description
#' Plot density plots for numeric results and bar plots for non-numeric results
#' of a Monte Carlo Simulation run by [future_mc()].
#'
#' @param x An object of class `mc`, for which holds `simple_output = TRUE`.
#' See value of [future_mc()].
#' @param join A character vector containing the `nice_names` for the different
#' parameter combinations (returned by [future_mc()]),
#' which should be plotted together.
#' Default: Each parameter combination is plotted distinctly.
#' @param which_setup A character vector containing the `nice_names`
#' for the different parameter
#' combinations (returned by [future_mc()]), which should be plotted.
#' Default: All parameter combinations are plotted.
#' @param parameter_comb Alternative to `which_setup`.
#' A named list whose components are named after
#' (some of) the parameters in `param_list` in [future_mc()]
#' and each component is a vector containing
#' the values for the parameters to filter by.
#' Default: All parameter combinations are plotted.
#' @param plot Boolean that specifies whether
#' the plots should be printed while calling the function or not.
#' Default: `TRUE`
#' @param ... ignored
#'
#'
#' @details Only one of the arguments `join`, `which_setup`, and `paramter_comb`
#' can be specified at one time.
#'
#' @return A list whose components are named after the outputs of `fun`
#' and each component
#' contains an object of class `ggplot` and `gg`
#' which can be plotted and modified with the
#' [ggplot2] functions.
#'
#' @export
#'
#' @examples
#'
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2)
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
#' returned_plot1 <- plot(test_mc)
#'
#' returned_plot1$mean +
#'  ggplot2::theme_minimal() +
#'  ggplot2::geom_vline(xintercept = 3)
#'
#' returned_plot2 <- plot(test_mc,
#' which_setup = test_mc$nice_names[1:2], plot = FALSE)
#' returned_plot2$mean
#'
#' returned_plot3 <- plot(test_mc,
#' join = test_mc$nice_names[1:2], plot = FALSE)
#' returned_plot3$mean
#'
plot.mc <-
  function(
    x, join = NULL,
    which_setup = NULL,
    parameter_comb = NULL,
    plot = TRUE,
    ...
  ){

    checkmate::assert_class(x, "mc")
    if(!x$simple_output){
      stop("fun has to return a list with named components.
         Each component has to be scalar.")
    }
    param_names <- names(x$parameter)
    stat_names <- dplyr::setdiff(names(x$output), c("params", param_names))
    setup_names <- unique(x$output$params)
    checkmate::assert_subset(join, setup_names, empty.ok = TRUE)
    checkmate::assert_subset(which_setup, setup_names, empty.ok = TRUE)
    checkmate::assert_list(parameter_comb, names = "named", null.ok = TRUE)
    checkmate::assert_subset(names(parameter_comb), param_names, empty.ok = TRUE)
    purrr::walk(
      parameter_comb,
      checkmate::assert_atomic_vector,
      unique = TRUE,
      .var.name = "Element of parameter_comb"
    )

    if(!is.null(which_setup) & !is.null(parameter_comb)){
      stop("Please subset the setups either with which_setup or parameter_comb,
         not with both!")
    }

    if(!is.null(which_setup) & !is.null(join)) {
      stop("Arguments which_setup and
         join cannot be specified at the same time!")
    }

    if(!is.null(parameter_comb) & !is.null(join)) {
      stop("Arguments parameter_comb and
         join cannot be specified at the same time!")
    }

    data_plot <-
      x$output

    if(!is.null(which_setup)) {
      data_plot <-
        data_plot %>%
        dplyr::filter(.data$params %in% which_setup)
    }

    if(!is.null(parameter_comb)){
      count <- 0
      data_plot <-
        data_plot %>%
        dplyr::filter(
          dplyr::if_all(
            names(parameter_comb),
            ~{
              count <<- count + 1
              .x %in% parameter_comb[[count]]
            }
          )
        )
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
            if(is.numeric(data_plot[[stat]]) & !all(is.na(data_plot[[stat]]))){
              plot_stat <-
                data_plot %>%
                ggplot2::ggplot(ggplot2::aes(.data[[stat]])) +
                ggplot2::geom_density() +
                ggplot2::facet_grid(~.data$params) +
                ggplot2::theme_bw()
              if(plot){
                print(plot_stat)
              }
              plot_stat
            } else if(!all(is.na(data_plot[[stat]]))){
              plot_stat <-
                data_plot %>%
                ggplot2::ggplot(ggplot2::aes(.data[[stat]])) +
                ggplot2::geom_bar() +
                ggplot2::facet_grid(~.data$params) +
                ggplot2::theme_bw()
              if(plot){
                print(plot_stat)
              }
              plot_stat
            }
          }
        ) %>%
        purrr::set_names(stat_names)

      plots_which <- plots_which[!purrr::map_lgl(plots_which, is.null)]

      return(invisible(plots_which))

    }

    if(!is.null(join)) {

      plots_joint <-
        purrr::map(
          stat_names,
          function(stat){
            if(is.numeric(data_plot[[stat]]) & !all(is.na(data_plot[[stat]]))){
              plot_stat <-
                data_plot %>%
                ggplot2::ggplot(
                  ggplot2::aes(
                    .data[[stat]],
                    col = .data[["params"]]
                  )
                ) +
                ggplot2::geom_density() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = stringr::str_c(
                  "Joint density plot of",
                  length(join),
                  "setups for the output",
                  stat, sep = " "
                ), color = "Setups") +
                ggplot2::theme(legend.position = "bottom")
              if(plot){
                print(plot_stat)
              }
              plot_stat

            }
          }
        ) %>%
        purrr::set_names(stat_names)

      plots_joint <- plots_joint[!purrr::map_lgl(plots_joint, is.null)]

      return(invisible(plots_joint))

    }

  }


#' Plot the summarized results of a Monte Carlo Simulation
#'
#' @description
#' Plot line plots of the path of the summarized output
#' over all simulation repetitions
#' of a Monte Carlo simulation run by
#' [future_mc()] and summarized by [summary.mc()]
#'
#' @param x An object of class `summary.mc`. For restrictions see details.
#' @param join A character vector containing the `nice_names` for the different
#' parameter combinations (returned by [future_mc()]),
#' which should be plotted together.
#' Default: Each parameter combination is plotted distinct.
#' @param which_setup A character vector containing the `nice_names`
#' for the different parameter
#' combinations (returned by [future_mc()]), which should be plotted.
#' Default: All parameter combinations are plotted.
#' @param parameter_comb Alternative to `which_setup`.
#' A named list whose components are named after
#' (some of) the parameters in `param_list` in [future_mc()]
#' and each component is a vector containing
#' the values for the parameters to filter by.
#' Default: All parameter combinations are plotted.
#' @param plot Boolean that specifies whether the plots
#' should be printed while calling the function or not.
#' Default: TRUE
#' @param ... additional arguments passed to callies.
#'
#' @details Only one of the arguments `join`, `which_setup`, and `paramter_comb`
#' can be specified at a time.
#'
#' A plot is only created for (output - parameter combination)-pairs
#' for which in [summary.mc()]
#' a function is provided in `sum_funs`
#' which returns a single numeric value and if the output
#' is included in `which_path`.
#'
#' @return A list whose components are named after the outputs of `fun`
#' and each component
#' contains an object of class `ggplot` and `gg` which can be plotted
#' and modified with the
#' [ggplot2] functions.
#'
#' @export
#'
#' @examples
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2)
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
#' returned_plot1 <- plot(summary(test_mc))
#'
#' returned_plot1$mean +
#'  ggplot2::theme_minimal()
#'
#' returned_plot2 <- plot(summary(test_mc),
#' which_setup = test_mc$nice_names[1:2], plot = FALSE)
#' returned_plot2$mean
#'
#' returned_plot3 <- plot(summary(test_mc),
#' join = test_mc$nice_names[1:2], plot = FALSE)
#' returned_plot3$mean
#'
plot.summary.mc <-
  function(
    x,
    join = NULL,
    which_setup = NULL,
    parameter_comb = NULL,
    plot = TRUE,
    ...
  ) {

    checkmate::assert_class(x, "summary.mc")
    setup_names <- names(x)
    stat_names <- names(x[[1]])
    checkmate::assert_subset(join, setup_names, empty.ok = TRUE)
    checkmate::assert_subset(which_setup, setup_names, empty.ok = TRUE)
    checkmate::assert_list(parameter_comb, names = "named", null.ok = TRUE)
    purrr::walk(
      parameter_comb,
      checkmate::assert_atomic_vector,
      unique = TRUE,
      .var.name = "Element of parameter_comb"
    )
    param_names <-
      names(x)[1] %>%
      stringr::str_replace_all(
        pattern = " ",
        replacement = ""
      ) %>%
      stringr::str_split(
        pattern = ","
      ) %>%
      unlist() %>%
      stringr::str_extract(
        pattern = "[^=]+"
      )
    checkmate::assert_subset(names(parameter_comb),
                             param_names,
                             empty.ok = TRUE)

    if(!is.null(which_setup) & !is.null(parameter_comb)){
      stop("Please subset the setups either with which_setup or
         parameter_comb, not with both!")
    }

    if(!is.null(parameter_comb) & !is.null(join)) {
      stop("Arguments parameter_comb and join cannot be
         specified at the same time!")
    }

    if(!is.null(which_setup) & !is.null(join)) {
      stop("Arguments which_setup and join cannot be
         specified at the same time!")
    }

    if(is.null(which_setup)){
      which_setup <- setup_names
    }

    if(!is.null(join)){
      which_setup <- join
    }

    . <- NULL

    data_plot <-
      purrr::map(
        stat_names,
        function(stat){
          stat_table <-
            purrr::map_dfc(
              which_setup,
              function(setup){
                if(checkmate::test_list(x[[setup]][[stat]],
                                        len = 2,
                                        names = "named")){
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
            tibble::rowid_to_column(var = "repetitions") %>%
            tidyr::pivot_longer(cols = which_setup,
                                names_to = "setup",
                                values_to = stat) %>%
            dplyr::filter(!is.na(get(stat)))

          if(!is.null(parameter_comb) & nrow(stat_table) != 0){
            count <- 0
            stat_table <-
              stat_table %>%
              data.frame(
                .,
                purrr::map_dfr(
                  .$setup,
                  function(params){
                    eval(
                      parse(
                        text = stringr::str_c(
                          "list(", params, ")",
                          sep = ""
                        )
                      )
                    )
                  }
                )
              ) %>%
              dplyr::filter(
                dplyr::if_all(
                  names(parameter_comb),
                  ~{
                    count <<- count + 1
                    .x %in% parameter_comb[[count]]
                  }
                )
              ) %>%
              dplyr::select(-param_names)
          }

          stat_table
        }
      ) %>%
      purrr::set_names(stat_names)

    data_plot <- data_plot[purrr::map_lgl(
      data_plot,
      function(data){
        !all(is.na(data[[3]]))
      }
    )]

    if(is.null(join)){

      plots_over_reps_which <-
        purrr::map(
          stat_names,
          function(stat){
            if(!is.null(data_plot[[stat]])){
              plot_stat <-
                data_plot[[stat]] %>%
                ggplot2::ggplot(
                  ggplot2::aes(
                    x = .data[["repetitions"]],
                    y = .data[[stat]]
                  )
                ) +
                ggplot2::geom_line() +
                ggplot2::facet_grid(~.data$setup) +
                ggplot2::theme_bw()
              if(plot){
                print(plot_stat)
              }
              plot_stat
            }
          }
        ) %>%
        purrr::set_names(stat_names)

      plots_over_reps_which <-
        plots_over_reps_which[!purrr::map_lgl(plots_over_reps_which, is.null)]

      return(invisible(plots_over_reps_which))

    }

    if(!is.null(join)){
      plots_over_reps_joint <-
        purrr::map(
          stat_names,
          function(stat){
            if(!is.null(data_plot[[stat]])){
              plot_stat <-
                data_plot[[stat]] %>%
                ggplot2::ggplot(
                  ggplot2::aes(
                    x = .data[["repetitions"]],
                    y = .data[[stat]],
                    col = .data[["setup"]]
                  )
                ) +
                ggplot2::geom_line() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = stringr::str_c(
                  "Joint time series of",
                  length(join),
                  "setups for the output",
                  stat,
                  sep = " "
                ), color = "Setups") +
                ggplot2::theme(legend.position = "bottom")
              if(plot){
                print(plot_stat)
              }
              plot_stat
            }
          }
        ) %>%
        purrr::set_names(stat_names)

      plots_over_reps_joint <-
        plots_over_reps_joint[!purrr::map_lgl(plots_over_reps_joint, is.null)]

      return(invisible(plots_over_reps_joint))
    }

  }

#' Print the results of a Monte Carlo Simulation
#'
#' @description
#' Print the results of a Monte Carlo Simulation run by [future_mc()]
#'
#' @param x An object of class `mc`.
#' @param ... ignored
#'
#' @return print shows a complete representation
#' of the run Monte Carlo Simulation
#'
#' @export
#'
#' @examples
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2)
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
#' test_mc

print.mc <-
  function(
    x,
    ...
  ){

    checkmate::assert_class(x, "mc")

    cat("Monte Carlo simulation results for the specified function: \n \n",
        stringr::str_c(deparse(x$fun), collapse = "\n", sep = " "),
        "\n \n", "The following",
        length(x$nice_names), "parameter combinations: \n")
    print(x$parameter)
    cat("are each simulated", x$repetitions, "times.",
        "\n \n The Running time was:",
        stringr::str_c(hms::as_hms(x$calculation_time)),
        "\n \n Parallel:", x$parallel,
        "\n \n The following parallelisation plan was used: \n")
    print(x$plan)
    cat("\n", "Seed:", x$seed)

  }




#' Print the summarized results of a Monte Carlo Simulation
#'
#' Print the summarized results of a Monte Carlo Simulation run by [future_mc()]
#' and summarized by [summary.mc()]
#'
#' @param x An object of class `summary.mc`
#' @param ... ignored
#'
#' @return print shows a nice representation of the
#' summarized results of a Monte Carlo Simulation
#'
#' @export
#'
#' @examples
#'
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, var = stat_2))
#' }
#'
#' param_list <- list(param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2)
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
#' summary(test_mc)
print.summary.mc <-
  function(
    x,
    ...
  ){

    checkmate::assert_class(x, "summary.mc")
    setup_names <- names(x)
    stat_names <- names(x[[1]])

    purrr::walk(
      stat_names,
      function(stat){
        cat("Results for the output ", stat, ": \n ", sep = "")
        purrr::walk(
          setup_names,
          function(setup){
            if(checkmate::test_number(x[[setup]][[stat]][[1]])){
              cat("  ", setup, ": ", x[[setup]][[stat]][[1]], " \n ", sep = "")
            } else {
              cat("  ", setup, ": \n", sep = "")
              print(x[[setup]][[stat]][[1]])
              cat("\n ")
            }
          }
        )
        cat("\n \n")
      }
    )
  }


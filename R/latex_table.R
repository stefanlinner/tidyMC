#' Create a 'LaTeX' table with the summarized results of a Monte Carlo Simulation
#'
#' Create a 'LaTeX' table containing the
#' summarized results of a Monte Carlo simulation run
#' by [future_mc()] and summarized by [summary.mc()].
#'
#' @param x An object of class `summary.mc`. For restrictions see details.
#' @param repetitions_set A vector of integers specifying
#' at which repetitions the summarized
#' results should be displayed in the table.
#' Default: The argument `repetitions` in [future_mc()],
#' which means that the summarized results after
#' the last repetition are displayed in the table.
#' @param which_setup A character vector containing the `nice_names`
#' for the different parameter
#' combinations (returned by [future_mc()]),
#' which should be presented in the table.
#' Default: All parameter combinations are presented.
#' @param parameter_comb Alternative to `which_setup`.
#' A named list whose components are named after
#' (some of) the parameters in `param_list` in [future_mc()].
#' Each component is a vector containing
#' the values for the parameters to be filtered by.
#' Default: All parameter combinations are presented.
#' @param which_out A character vector containing
#' the names of (some of) the named outputs
#' (the names of the returned list of `fun` in [future_mc()]),
#' which should be displayed in the table.
#' Default: All outputs are displayed.
#' @param kable_options A list whose components are named
#' after possible parameters
#' of [kableExtra::kbl()] customizing the generated table.
#' @details Only one of the arguments `which_setup` and `parameter_comb`
#' can be specified at one time.
#'
#' Only (output - parameter combination)-pairs
#' for which the summary function specified
#' in the `sum_funs` argument of [summary.mc()]
#' returns a single scalar value appear as
#' non-`NA` values in the 'LaTeX' table.
#' If a specific output is summarized with functions
#' that do not return a single numeric value over all parameter combinations,
#' then this output is discarded from the table.
#' Similarly, if for a specific parameter combination all `fun` outputs are
#' summarized with functions which do not return a single numeric value,
#' then this parameter combination is discarded as well.
#' In summary, all outputs must be summarized with functions
#' which return just one numeric value.
#'
#' @return An object of class `knitr_kable`
#' which can be modified by the functions
#' in the [kableExtra] package is returned.
#' @export
#'
#' @examples
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
#' test_mc <- future_mc(
#'   fun = test_func,
#'   repetitions = 1000,
#'   param_list = param_list,
#'   n = 10,
#'   x2 = 2
#' )
#'
#' tidy_mc_latex(summary(test_mc))
#'
#' set.seed(101)
#' tidy_mc_latex(
#'   summary(test_mc),
#'   repetitions_set = c(10, 1000),
#'   which_out = "mean",
#'   kable_options = list(caption = "Mean MCS results")
#' )
#'
tidy_mc_latex <-
  function(
    x,
    repetitions_set = NULL,
    which_setup = NULL,
    parameter_comb = NULL,
    which_out = NULL,
    kable_options = NULL
  ) {
    checkmate::assert_class(x, "summary.mc")
    setup_names <- names(x)
    stat_names <- names(x[[1]])
    checkmate::assert_integerish(repetitions_set, lower = 1, null.ok = TRUE)
    checkmate::assert_subset(which_setup, setup_names, empty.ok = TRUE)
    checkmate::assert_subset(which_out, stat_names, empty.ok = TRUE)
    checkmate::assert_list(kable_options, null.ok = TRUE)
    if (is.null(kable_options)) {
      kable_options <- list()
    }
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
                             param_names, empty.ok = TRUE)

    if (!is.null(which_setup) & !is.null(parameter_comb)) {
      stop("Please subset the setups either with
         which_setup or parameter_comb, not with both!")
    }

    if (is.null(which_setup)) {
      which_setup <- setup_names
    }

    if (is.null(which_out)) {
      which_out <- stat_names
    }

    n_reps <- attributes(x)$n_reps

    if (is.null(repetitions_set)) {
      repetitions_set <- n_reps
    }

    n_setups <- length(setup_names)

    . <- NULL

    data_table <-
      purrr::map_dfr(
        which_setup,
        function(setup) {
          stat_dat_setup <-
            purrr::map_dfc(
              which_out,
              function(stat) {
                if (checkmate::test_set_equal(repetitions_set, n_reps)) {
                  if (checkmate::test_list(x[[setup]][[stat]],
                                           len = 2,
                                           names = "named"
                  )) {
                    stat_dat <- list(x[[setup]][[stat]][[2]])
                    names(stat_dat) <- stat
                    return(stat_dat)
                  } else if (checkmate::test_number(x[[setup]][[stat]][[1]])) {
                    stat_dat <- list(rep(x[[setup]][[stat]][[1]], n_reps))
                    names(stat_dat) <- stat
                    return(stat_dat)
                  } else {
                    stat_dat <- list(NA)
                    names(stat_dat) <- stat
                    return(stat_dat)
                  }
                }

                if (!checkmate::test_set_equal(repetitions_set, n_reps)) {
                  if (checkmate::test_list(x[[setup]][[stat]],
                                           len = 2,
                                           names = "named")) {
                    stat_dat <- list(x[[setup]][[stat]][[2]])
                    names(stat_dat) <- stat
                    return(stat_dat)
                  } else {
                    stat_dat <- list(NA)
                    names(stat_dat) <- stat
                    return(stat_dat)
                  }
                }
              }
            ) %>%
            tibble::rowid_to_column(var = "repetitions")
          stat_dat_setup$setup <- setup
          stat_dat_setup
        }
      ) %>%
      dplyr::select(which(colMeans(is.na(.)) != 1)) %>%
      dplyr::filter(.data$repetitions %in% repetitions_set) %>%
      data.frame(
        .,
        purrr::map_dfr(
          .$setup,
          function(params) {
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
      dplyr::relocate(
        names(.) %>%
          intersect(which_out),
        .after = dplyr::last_col()
      )

    if (!is.null(parameter_comb)) {
      count <- 0
      data_table <-
        data_table %>%
        dplyr::filter(
          dplyr::if_all(
            names(parameter_comb),
            ~ {
              count <<- count + 1
              .x %in% parameter_comb[[count]]
            }
          )
        )
    }

    if (!any(stat_names %in% names(data_table))) {
      stop("No output variable contained in table!
         Please check which_path or sum_funs of summary function")
    }

    n_setups_contained <- length(unique(data_table$setup))

    default_kable_options <-
      list(
        x = data_table %>%
          dplyr::arrange(.data$repetitions) %>%
          dplyr::select(-dplyr::all_of(c("setup", "repetitions"))),
        format = "latex",
        booktabs = TRUE,
        digits = 3,
        align = "c",
        caption = "Monte Carlo simulations results",
        escape = FALSE
      )
    kable_options <- utils::modifyList(default_kable_options, kable_options)

    out <- do.call(kableExtra::kbl, kable_options) %>%
      kableExtra::add_footnote(
        label = stringr::str_c(
          "Total repetitions = ", n_reps,
          ", total parameter combinations = ", n_setups,
          collapse = ",", sep = ""
        ),
        notation = "none",
        escape = FALSE
      )

    if (!checkmate::test_set_equal(repetitions_set, n_reps)) {
      index <- rep(n_setups_contained, length(repetitions_set))
      names(index) <- stringr::str_c("N = ", repetitions_set, sep = "")
      out <-
        kableExtra::pack_rows(
          kable_input = out,
          index = index
        )
    }

    return(out)
  }

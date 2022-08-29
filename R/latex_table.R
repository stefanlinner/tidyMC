#' Create a LaTeX table with the summarized results of a Monte Carlo Simulation
#'
#' Create a LaTeX table containing the summarized results of a Monte Carlo simulation run
#' by [future_mc()] and summarized by [summary.mc()].
#'
#' @param x An object of class `summary.mc`. For restrictions see details.
#' @param repetitions_set A vector of integers specifying at which repetitions the summarized
#' results should be displayed in the table.
#' Default: The argument `repetitions` in [future_mc()], which means that the summarized results after
#' the last repetition are displayed in the table.
#' @param which_setup A character vector containing the `nice_names` for the different parameter
#' combinations (returned by [future_mc()]), which should be presented in the table.
#' Default: All parameter combinations are presented.
#' @param parameter_comb Alternative to which_setup. A named list whose components are named after
#' (some of) the parameters in `param_list` in [future_mc()]. Each component is a vector containing
#' the values for the parameters to be filtered by.
#' Default: All parameter combinations are presented.
#' @param which_out A character vector containing the names of (some of) the named outputs
#' (the names of the returned list of `fun` in [future_mc()]), which should be displayed in the table.
#' Default: All outputs are displayed.
#' @param caption A string specifying the caption of the latex table.
#' @param column_names Column names for the resulting table. This vector must have the same size as
#' the number of outputs of `fun` and parameters in `param_list` They can be written
#' in standard LaTeX manner.
#'
#' @details Only one of the arguments `which_setup` and `parameter_comb` can be specified
#' at one time.
#'
#' Only (output - parameter combination)-pairs for which the summary function specified
#' in the `sum_funs` argument of [summary.mc()] returns a single scalar value appear as
#' non-`NA` values in the latex table. If a specific output is summarized with functions
#' which do not return a single numeric value over all parameter combinations, then this output
#' is discarded from the table. Similarly, if for a specific parameter combination all `fun` outputs are
#' summarized with functions which do not return a single numeric value, then this parameter combination
#' is discarded as well. In summary, all outputs must be summarized with functions
#' which return just one numeric value.
#'
#' @return An object of class `knitr_kable` which can be modified by the functions
#' in the `kableExtra` package is returned and printed in the console. See`kableExtra`
#' @export
#'
#' @examples
#' test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){
#'
#'   data <- rnorm(n, mean = param) + x1 + x2
#'   stat <- mean(data)
#'   stat_2 <- var(data)
#'   test <- LETTERS[sample(1:26, 1)]
#'
#'   if(x3 == 0 & x4 == 0){
#'     next
#'   }
#'
#'   if (x2 == 5){
#'     stop("x2 can't be 5!")
#'   }
#'
#'   return(list(mean = stat, sd = stat_2, test = test))
#' }
#'
#'
#' param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
#'                    x1 = 1:2, x2 = 2)
#'
#'
#' test <- future_mc(fun = test_func, repetitions = 1000, param_list = param_list)
#'
#' tidy_mc_latex(summary(test))
#'

tidy_mc_latex <- function(
    x,
    repetitions_set = NULL,
    which_setup = NULL,
    parameter_comb = NULL,
    which_out = NULL,
    caption = "Monte Carlo simulations results",
    column_names = NULL# HUHU: Allow for more options which are passed to kable?
){

  checkmate::assert_class(x, "summary.mc")
  setup_names <- names(x)
  stat_names <- names(x[[1]])
  checkmate::assert_integerish(repetitions_set, lower = 1, null.ok = TRUE)
  checkmate::assert_subset(which_setup, setup_names, empty.ok = TRUE)
  checkmate::assert_subset(which_out, stat_names, empty.ok = TRUE)
  checkmate::assert_list(parameter_comb, names = "named", null.ok = TRUE)
  checkmate::assert_character(column_names, null.ok = T)
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
  checkmate::assert_subset(names(parameter_comb), param_names, empty.ok = TRUE)

  if(!is.null(which_setup) & !is.null(parameter_comb)){
    stop("Please subset the setups either with which_setup or parameter_comb, not with both!")
  }

  if(is.null(which_setup)){
    which_setup <- setup_names
  }

  if(is.null(which_out)){
    which_out <- stat_names
  }

  n_reps <- attributes(x)$n_reps

  if(is.null(repetitions_set)){
    repetitions_set  <- n_reps
  }

  n_setups <- length(setup_names)

  data_table <-
    purrr::map_dfr(
      which_setup,
      function(setup){
        stat_dat_setup <-
          purrr::map_dfc(
            which_out,
            function(stat){

              if(checkmate::test_set_equal(repetitions_set, n_reps)){
                if(checkmate::test_list(x[[setup]][[stat]], len = 2, names = "named")){
                  stat_dat <- list(x[[setup]][[stat]][[2]])
                  names(stat_dat) <- stat
                  return(stat_dat)
                } else if(checkmate::test_number(x[[setup]][[stat]][[1]])) {
                  stat_dat <- list(rep(x[[setup]][[stat]][[1]], n_reps))
                  names(stat_dat) <- stat
                  return(stat_dat)
                } else {
                  stat_dat <- list(NA)
                  names(stat_dat) <- stat
                  return(stat_dat)
                }
              }

              if(!checkmate::test_set_equal(repetitions_set, n_reps)){
                if(checkmate::test_list(x[[setup]][[stat]], len = 2, names = "named")){
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
    cbind(
      .,
      purrr::map_dfr(
        .$setup,
        function(params){
          eval(
            parse(
              text = paste(
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

  if(!is.null(parameter_comb)){
    count <- 0
    data_table <-
      data_table %>%
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

  if(!any(stat_names %in% names(data_table))){
    stop("No output variable contained in table! Please check which_path or sum_funs of summary function")
  }

  n_setups_contained <- length(unique(data_table$setup))

  if(!is.null(column_names)){
    if (length(column_names)!=length(c(stat_names, param_names))){
      stop("Length of column names differ from the number of columns in the table")
    } else {
      colnames(data_table)[which(colnames(data_table) %in% c(param_names, stat_names))] <- column_names
    }
  }
  out <- data_table %>%
    dplyr::arrange(.data$repetitions) %>%
    dplyr::select(-.data$setup, -.data$repetitions) %>%
    kableExtra::kbl(format = "latex", booktabs = TRUE,
                    digits = 3,
                    align = "c", caption = caption, escape = FALSE) %>%
    kableExtra::footnote(general = paste("Total repetitions = ",n_reps,
                                         ", total parameter combinations = ", n_setups,
                                         collapse = ",", sep = ""))

  if(!is.null(repetitions_set)){
    out <- eval(parse(text =
                        paste("kableExtra::pack_rows(kable_input = out, index = c(",
                              paste( "\"N = ", repetitions_set,
                                     "\" = ", rep(n_setups_contained, length(repetitions_set)), collapse = ",", sep = ""),
                              "))", collapse = "\n")
    )
    )
  }

  return(out)
}


#' Latex table output for summary.mc object
#'
#' @param repetitions_set test
#' @param caption test
#' @param parameter_comb test
#' @param x test
#' @param which_setup test
#' @param which_stat test
#'
#' @return return latex_table
#' @export
#'
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

tidy_mc_latex <- function(x,
                          repetitions_set = NULL,
                          which_setup = NULL,
                          which_stat = NULL,
                          parameter_comb = NULL,
                          caption = "Monte Carlo simulations results"
){

  checkmate::assert_class(x, "summary.mc")
  setup_names <- names(x)
  stat_names <- names(x[[1]])
  checkmate::assert_integerish(repetitions_set, lower = 1, null.ok = TRUE)
  checkmate::assert_subset(which_setup, setup_names, empty.ok = TRUE)
  checkmate::assert_subset(which_stat, stat_names, empty.ok = TRUE)
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
  checkmate::assert_subset(names(parameter_comb), param_names, empty.ok = TRUE)

  if(!is.null(which_setup) & !is.null(parameter_comb)){
    stop("Please subset the setups either with which_setup or parameter_comb, not with both!")
  }

  if(is.null(which_setup)){
    which_setup <- setup_names
  }

  if(is.null(which_stat)){
    which_stat <- stat_names
  }

  n_reps <- NULL
  n_setups <- length(setup_names)

  data_table <-
    purrr::map_dfr(
      which_setup,
      function(setup){
        stat_dat_setup <-
          purrr::map_dfc(
            which_stat,
            function(stat){
              if(checkmate::test_list(x[[setup]][[stat]], len = 2, names = "named")){
                if(is.null(n_reps)){
                  n_reps <<- length(x[[setup]][[stat]][[2]])
                }
                if(is.null(repetitions_set)){
                  repetitions_set <<- length(x[[setup]][[stat]][[2]])
                }
                stat_dat <- list(x[[setup]][[stat]][[2]])
                names(stat_dat) <- stat
                return(stat_dat)
              } else {
                stat_dat <- list(NA)
                names(stat_dat) <- stat
                return(stat_dat)
              }
            }
          ) %>%
          tibble::rowid_to_column(var = "Replications")
        stat_dat_setup$setup <- setup
        stat_dat_setup
      }
    ) %>%
    dplyr::select(which(colMeans(is.na(.)) != 1)) %>%
    dplyr::filter(.data$Replications %in% repetitions_set) %>%
    cbind(
      ., #.
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
      names(.data) %>% #.
        intersect(which_stat),
      .after = dplyr::last_col()
    )

  if(!is.null(parameter_comb)){
    count <- 0
    data_table <-
      data_table %>%
      dplyr::filter(
        dplyr::across(
          names(parameter_comb),
          ~{
            count <<- count + 1
            .x %in% parameter_comb[[count]]
          }
        )
      )
  }


  out <- data_table %>%
    dplyr::arrange(.data$Replications) %>%
    dplyr::select(-.data$setup, -.data$Replications) %>%
    kableExtra::kbl(format = "latex", booktabs = T,
                    digits = 3,
                    align = "c", caption = caption) %>%
    kableExtra::footnote(general = paste("Total repetitions = ",n_reps,
                                         ", total parameter combinations = ", n_setups,
                                         collapse = ",", sep = ""))

  if(!is.null(repetitions_set)){
    out <- eval(parse(text =
                        paste("kableExtra::pack_rows(kable_input = out, index = c(",
                              paste( "\"N = ", repetitions_set,
                                     "\" = ", rep(n_setups, length(repetitions_set)), collapse = ",", sep = ""),
                              "))", collapse = "\n")
    )
    )
  }

  print(out) # Needed?
  return(out)
}

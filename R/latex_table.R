#' Latex table output for summary.mc object
#'
#' @param object test
#' @param sum_funs test
#' @param repetitions_set test
#' @param caption test
#' @param parameter_comb test
#'
#' @return return latex_table
#' @export
#'
#' @importFrom magrittr %>%
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
#' tidy_mc_latex(test)

tidy_mc_latex <- function(object,
                          sum_funs = NULL,
                          repetitions_set = NULL,
                          caption = "Monte Carlo simulations results",
                          parameter_comb = NULL){

  checkmate::assert_class(object, "mc")
  if(!object$simple_output){
    stop("fun has to return a list with named components. Each component has to be scalar.")
  }


  if(is.null(sum_funs)){
    sum_funs <- list(mean = mean, sd = sd, test = table)
  }
  sum_test <- summary(object, sum_funs = sum_funs)

  num_res <- object$n_results
  num_com <- length(sum_test)

  aux <- c()
  count <- object$n_results * num_com
  for (i in 1:num_com){
    for (j in 1:num_com){
      if (length(sum_test[[i]][[j]]) != 2){
        count <- count - 1
        next
      } else {

        if (is.null(repetitions_set)) {
          repetitions_set <- length(sum_test[[i]][[j]][[2]])
        }
        aux <- rbind(aux, sum_test[[i]][[j]][[2]][repetitions_set])
      }
    }
  }

  reps <- length(repetitions_set)
  aux2 <- data.frame()
  count <- count/num_com
  in_1 <- seq(from = 1, to = nrow(aux), by = count)
  in_2 <- seq(from = count, to = nrow(aux), by = count)
  for (i in 1:reps){
    for (j in 1:num_com){
      aux2 <- rbind(aux2, t(aux[c(in_1[j], in_2[j]), i]))
    }
  }
  in_3 <- seq(from = 1, to = nrow(aux2), by = reps)
  iden <- unlist(lapply(X =  sum_test[[1]], FUN = function(x) (length(x)==2)))

  aux2 <- round(aux2, 3)
  # nice_names <- paste(rep(names(sum_test), reps))
  #
  # aux2 <- cbind(nice_names, aux2)

  # colnames(aux2) <- c("Parameters",
  #                     names(unlist(lapply(
  #                       X =  sum_test[[1]],
  #                       FUN = function(x) (length(x)==2))))[iden])

  aux2 <- cbind(purrr::map_dfr(
    seq_len(reps),
    function(x) object$parameter
  ), aux2)

  colnames(aux2) <- c(colnames(object$parameter),
                      names(unlist(lapply(X = sum_test[[1]], FUN = function(x) (length(x)==2))))[iden])

  # aux2 <- rbind(aux2,
  #               c("Total repetitions", object$n_results, rep("", ncol(aux2)-2+ncol(object$parameter))),
  #               c("Total parameter combinations", length(unique(object$setups)), rep("", ncol(aux2)-2+ncol(object$parameter))),
  #               c("Seed", object$seed, rep("", ncol(aux2)-2+ncol(object$parameter))))



  if(!is.null(parameter_comb)){
    filters <- stringr::str_replace_all(string = sub(
                                        x = sub(
                                          pattern = "list\\(",
                                          replacement = "",
                                          x = (deparse(parameter_comb)
                                          )
                                        ), replacement = "", pattern = "\\)"),
                                        pattern = "=", replacement = "==")
    eval(parse(
      text = paste("aux2 <- dplyr::filter(aux2,",
                            filters, ")",
                            sep = "",
                            collapse = ","
                   )
               )
         )
    num_com <- length(
      unique(
        apply(
        X = aux2[,1:ncol(object$parameter)],
        MARGIN = 1,
        FUN = function(x)paste(x, sep = "", collapse = ",")
        )
      )
    )
  }

  out <- aux2 %>%
    kableExtra::kbl(format = "latex", booktabs = T,
        digits = 3,
        align = "c", caption = caption) %>%
    kableExtra::footnote(general = paste("Total repetitions = ", object$n_results,
                                           ", total parameter combinations = ", length(unique(object$setups)),
                                           ", seed:", object$seed, ".",
                             collapse = ",", sep = ""))

  if (reps > 1){
    out <- eval(parse(text =
                        paste("kableExtra::pack_rows(kable_input = out, index = c(",
                              paste( "\"N = ", repetitions_set,
                                     "\" = ", rep(num_com, reps), collapse = ",", sep = ""),
                              "))", collapse = "\n")
    )
    )
  }

  print(out)
  return(out)

}



# Filter the parameters from the table


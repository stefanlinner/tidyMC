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
  checkmate::assert_list(sum_funcs, null.ok = TRUE)
  if(!is.null(sum_funcs)){
    checkmate::assert_choice(length(sum_funcs), c(1, nrow(object$sim_setups)))
  }

  if(is.null(sum_funcs)){

    object$output %>%
      dplyr::group_by(.data$params) %>%
      dplyr::group_map(~{
        purrr::map_dfr(.$results$value,
                function(.x){

                  purrr::walk(
                    .x,
                    function(func_list_outputs){
                          checkmate::assert_scalar(func_list_outputs)
                    }
                  )

                  .x
                }) %>%
          summary()

      }) %>%
      purrr::set_names(object$sim_setups$setup)

  }

}

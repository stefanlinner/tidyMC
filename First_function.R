### First functions ####

library(tidyverse)
library(purrr)
library(furrr)
library(parallelly)
library(codetools)
library(rlang)
library(MonteCarlo)
library(rlang)
library(checkmate)
library(magrittr)
library(stringr)

# Montecarlo function

parallelly::availableCores()


cores_number <- 4
repetitions <- 10

test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 2){
  
  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)
  
  if (x2 == 5){
    stop("x2 can't be 5!")
  }
  
  return(list(stat, stat_2))
}


param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.5),
                   x1 = 1:2, x2 = 2)

future_mc <- 
  function(
    test_func,
    repetitions,
    param_list,
    packages = "test",
    parallelisation_plan = NULL, 
    simple = FALSE
    # ,test = TRUE
  ){
    
    # Asserting inputs
    checkmate::assert_function(test_func, args = names(param_list))
    checkmate::assert_int(repetitions, lower = 1)
    checkmate::assert_list(param_list)
    purrr::walk(seq_along(param_list), checkmate::assert_vector)
    checkmate::assert_character(packages)
    checkmate::assert_list(parallelisation_plan, null.ok = TRUE)
    checkmate::assert_logical(simple, len = 1)
    
    if(is.null(parallelisation_plan)){
      parallelisation_plan <- 
        list(
          strategy = multisession, 
          substitute = TRUE,
          .skip = FALSE,
          .call = TRUE,
          .cleanup = TRUE, 
          .init = TRUE
        )
    }
    
    do.call(plan, parallelisation_plan)
    # plan(multisession)
    
    # Order parameter names to match function order
    func_argnames <- formalArgs(test_func)
    # func_argnames <- names(as.list(args(test_func)))
    # func_argnames <- func_argnames[-which(func_argnames == "")]
    # func_argnames_equal <- paste(func_argnames, " = ", sep = "")
    param_names <- names(param_list)
    param_list <- param_list[order(match(func_argnames, param_names))]
    param_names <- names(param_list)
    
    
    # Create grid containers, maybe useless if we just extract it from the function
    # n_param<-length(param_list)
    # dim_vec<-numeric(n_param) 
    # for(i in seq_along(param_list)){
    #   dim_vec[i]<-length(param_list[[i]])
    # } 
    # containers <- character(n_param)
    # for(i in seq_along(param_list)){
    #   (assign(paste(param_names[i],"_grid",sep=""),param_list[[i]])) # create grid for each parameter
    #   containers[i] <-  paste(param_names[i],"_grid",sep="")
    # }
    
    # Create grid tibble
    # containers <- paste(containers, collapse = ",", sep = "")
    # aux <- eval(parse(text = paste("expand.grid(", containers, ");", sep = "")))
    # colnames(aux) <- param_names
    # grid_size <- nrow(aux)
    
    
    
    # Rewriting above part
    n_param <- length(param_list)
    aux <- as_tibble(
      expand.grid(param_list)
    )
    grid_size <- nrow(aux)
    
    
    
    
    # Aux but for the number of repetitions could be improved / future needed? 
    aux2 <- purrr::map_dfr(seq_len(repetitions), function(x) aux)
    
    # Nice names for the parameters
    nice_names <- 
      purrr::map_chr(
        seq_len(nrow(aux2)),
        function(.x){
          paste(
            purrr::map_chr(
              names(aux2), 
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
        })
    
    
    # New function based on test_func that gives us the error message with the
    # function call
    
    test_func_2 <- args(test_func)
    body(test_func_2, envir = environment()) <- 
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
                    paste("test_func(", 
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
              #stop( I don't want early exit -> We prefer test-run (alternatively if test = F, then with stop!)
              paste(
                " \n Function error: ", eval(
                  parse(
                    text =
                      paste("unlist(rlang::catch_cnd(test_func(", 
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
        .f = test_func_2,
        .progress = TRUE,
        .options = furrr_options(seed = TRUE)
      )
    
    test_runs_errors <- unlist(test_runs) %>% 
      stringr::str_subset(pattern = "^ \n Function error")
    
    if(length(test_runs_errors != 0)){
      stop(test_runs_errors)
    } else {
      message("Test-run successfull: No errors occurred!")
    }
    
    # Check the number of results
    num_res <- length(unlist(furrr::future_pmap(.l = aux2[1,],
                                                .f = test_func_2,
                                                .options = furrr_options(seed = TRUE))))
    
    
    # Results
    
    message("Running whole simulation...")
    
    results_list <- 
      furrr::future_pmap(
        .l = aux2, 
        .f = test_func_2, 
        .progress = TRUE,
        .options = furrr_options(seed = TRUE))
    
    message("Simulation was successfull!")
    
    # Extract results
    
    if(simple){
      
      res_names <- purrr::map_chr(seq_len(num_res), function(.x){
        paste("res_", .x, sep = "")
      })
      
      out <- purrr::map_dfr(results_list, function(.x){
        res <- .x
        names(res) <- res_names
        res
      })
      
      out <- as_tibble(cbind(param = nice_names, out))
      
    } else {
      out <- tibble(params =  nice_names,
                    results = as_tibble_col(results_list))
    }
    
    plan("default")
    return(out)
  }


test <- future_mc(test_func = test_func, repetitions = 1000, param_list = param_list, simple = T)

param_list <- list(n = 1000, param = seq(from = 0, to = 1, by = 0.5),
                   x1 = 1:2, x2 = 1:4)



#### Things to implement still 

#* I need to extract the packages names from the functions we call, and then
#* supply them to the furrr_options. The code below does that, but i need to 
#* change it so it's not a complete rip off.





all<-ls(name=.GlobalEnv)
all<-backtick_binaries(all)
all_funcs<-NULL
for(i in 1:length(all)){
  if(is.function(eval(parse(text=all[i])))){all_funcs<-c(all_funcs,all[i])}
}
globals_in_func<-findGlobals(test_func, merge=FALSE)
in_func_aux<-globals_in_func$functions # extract functions used in test_func

# --------------   remove <- from function name for replacement functions

in_func_aux2<-gsub("<-","",in_func_aux)
in_func_aux<-backtick_binaries(in_func_aux2[in_func_aux2!=""])

####################################################

in_func<-NULL # loop to sort out primitive functions
for(i in 1:length(in_func_aux)){if(is.function(tryCatch(.Primitive(in_func_aux[i]), error=function(e)FALSE))==FALSE){in_func<-c(in_func,in_func_aux[i])}}

new_funcs<-subset(in_func,in_func%in%all_funcs)
export_functions<-new_funcs

# -- loop through deeper functions to find functions called by functions in test_func and deeper nested functions

all_funcs_found<-in_func # create array of names of all functions found so that the packages required can be found below
while(length(new_funcs)>0){ # while loop runs as long as new functions are found in deeper layers 
  n_exp<-length(new_funcs)
  new_funcs2<-NULL
  for(i in 1:n_exp){# find functions used in every new function
    in_inner_aux<-findGlobals(eval(parse(text=new_funcs[i])))  
    in_inner<-NULL # loop to sort out primitive functions
    for(i in 1:length(in_inner_aux)){
      if(is.function(tryCatch(.Primitive(in_inner_aux[i]), error=function(e)FALSE))==FALSE){
        if(tryCatch(is.function(eval(parse(text=in_inner_aux[i]))), error=function(e)FALSE)){
          in_inner<-c(in_inner,in_inner_aux[i])
        }
      }
    }
    new_funcs2<-c(new_funcs2,subset(in_inner,in_inner%in%all_funcs)) # list those new functions found that are defined in global environment
    if(length(in_inner)>0){all_funcs_found<-unique(c(all_funcs_found,in_inner[apply(as.matrix(in_inner),1,exists)]))} # determine which of the functions defined in inner functions do exist and append to list of all functions that may come from packages
  }
  new_funcs<-new_funcs2
  export_functions<-c(export_functions,new_funcs)
}



# -- add everything that is specified in export_also

globals_in_func$variables<-globals_in_func$variables[which(globals_in_func$variables%in%c("LETTERS","letters","month.abb", "month.name","pi")==FALSE)]

export_functions<-c(export_functions,export_also$functions,export_also$data, export_also$variables, globals_in_func$variables)

# -------- Find out which packages have to be loaded into cluster

packages<-NULL
if(is.null(all_funcs_found)==FALSE){
  all_env<-search() #list all environments
  env_names<-unlist(strsplit(all_env[grep(":", all_env)], split=":")) # keep only those environments that refer to packages
  env_names<-env_names[-which(env_names=="package")]
  
  #loop through non-primitive functions used in func and check from which package they are
  for(i in 1:length(all_funcs_found)){
    if(environmentName(environment(eval(parse(text=all_funcs_found[i]))))%in%env_names){
      packages<-c(packages,env_names[which(env_names==environmentName(environment(eval(parse(text=all_funcs_found[i])))))])
    }
  }
  packages<-unique(packages[packages!="base"])
  
  dependencies_list<-NULL # loop through packages found and collect their dependencies in character vector
  if(length(packages)>0){
    for(i in 1:length(packages)){
      dependencies_list<-c(dependencies_list,unlist(strsplit(as.character(packageDescription(packages[i])$Depends), split=",")))
    }
    dependencies_list<-unique(dependencies_list)
    dependencies_list<-gsub(" ","",dependencies_list)
    sort_out<-which(packages%in%dependencies_list)
    if(length(sort_out)>0){packages<-packages[-sort_out]}  # keep only those packages that are not automatically included because they are dependencies
  }
}



# summary functions -> list of function for each result in list
# What to do with the furrr.options?
# Nice output -> look in tidyverse package



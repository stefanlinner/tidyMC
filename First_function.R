### First functions ####

library(tidyverse)
library(purrr)
library(furrr)
library(parallelly)
library(codetools)
library(rlang)
library(MonteCarlo)

# Montecarlo function

parallelly::availableCores()


cores_number <- 6
repetitions <- 10

test_func <- function(param = 0.1, n = 100){
  
  data <- rnorm(n, mean = param)
  
  stat <- mean(data)
  stat_2 <- var(data)
  return(list(stat, stat_2))
}

param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.1))

MCMC_func <- function(test_func, repetitions, param_list, packages,
                      cores_number, simple = FALSE){
  plan(multisession, workers = cores_number) 
  # Order parameter names to match function order
  func_argnames <- names(as.list(args(test_func)))
  func_argnames <- func_argnames[-which(func_argnames == "")]
  param_names <- names(param_list) 
  param_list <- param_list[order(match(func_argnames, param_names))]
  param_names <- names(param_list) 
  
  
  # Create grid containers, maybe useless if we just extract it from the function
  n_param<-length(param_list)                                                       
  dim_vec<-numeric(n_param) 
  for(i in 1:n_param){
    dim_vec[i]<-length(param_list[[i]])
  } 
  containers <- character(n_param)                                              
  for(i in 1:n_param){
    (assign(paste(param_names[i],"_grid",sep=""),param_list[[i]])) # create grid for each parameter
    containers[i] <-  paste(param_names[i],"_grid",sep="")
  }
  
  # Create grid tibble
  containers <- paste(containers, collapse = ",", sep = "")
  aux <- eval(parse(text = paste("expand.grid(", containers, ");", sep = "")))
  aux <- as_tibble(aux)
  colnames(aux) <- param_names
  grid_size <- nrow(aux)
  
  
  
  # Check the number of results
  num_res <- length(unlist(pmap(.l = aux[1,], .f = test_func)))
  
  # Aux but for the number of repetitions could be improved
  aux2 <- future_map_dfr(seq_len(repetitions), function(x) aux)
  
  # Results
  results_list <- future_pmap(.l = aux2, .f = test_func, 
                              .options = furrr_options(seed = TRUE))
  
  # Parameter names
  
  
  # Extract results
  
  if (simple == TRUE){
    
    out <- eval(parse(text = paste("data.frame(", 
                                   paste("res_", 1:num_res, "=",
                                         rep("numeric(grid_size)",
                                             num_res),
                                         sep = "", collapse = ", "), ")", sep = "")))
    
    for (i in 1:length(results_list)){
      for (j in 1:num_res){
        out[i,j] <- results_list[[i]][j]
      }
    }
  } else if (simple == FALSE){
    # This still gives the parameters quite ugly, I will include proper names
    # like "n=1, param =2"
    out <- tibble(params =  apply(aux2, 1, function(x)paste(x, collapse = " , ", sep = "")),
                  results = as_tibble_col(results_list))
  }
  
  
  
  
  plan("default")
  return(out)
}

test <- MCMC_func(test_func = test_func, repetitions = 1000, param_list = param_list,
                  cores_number = 7, simple = FALSE)



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


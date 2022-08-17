### First functions ####
#
# library(tidyverse)
# library(parallelly)
# library(codetools)
# library(MonteCarlo)


# parallelly::availableCores()
#
#
# cores_number <- 4
# repetitions <- 10

test_func <- function(param = 0.1, n = 100, x1 = 1, x2 = 5, x3 = 1, x4 = 6){

  data <- rnorm(n, mean = param) + x1 + x2
  stat <- mean(data)
  stat_2 <- var(data)
  test <- LETTERS[sample(1:26, 1)]

  if(x3 == 0 & x4 == 0){
    next
  }

  if (x2 == 5){
    stop("x2 can't be 5!")
  }

  return(list(mean = stat, sd = stat_2, test = test))
}


param_list <- list(n = 10, param = seq(from = 0, to = 1, by = 0.2),
                   x1 = 1, x2 = c(2,3,4))

devtools::load_all()

set.seed(101)
test1 <- future_mc(fun = test_func, repetitions = 20000, param_list = param_list, x3 = 6, x4 = 1, check = TRUE)












# default summary function --> summary (compatible with any data type)
summary(test1)

# user_defined summary function for different results
summary(test1, sum_funcs = list(mean = mean, sd = sd, test = table))

# user_defined summary function for different results and different setups
sum_funcs <- list(
  list(
    mean = mean, sd = sd, test = table
  ),
  list(
    mean = summary, sd = summary, test = table
  ),
  list(
    mean = max, sd = min, test = summary
  )
)

names(sum_funcs) <- test1$setups

summary(test1, sum_funcs = sum_funcs)




# summary changes:
# default for numeric --> mean, else keep summary
# do time series of mean over repitions

# ggplot2 informative legends with grid-package, gtable (?)


# non-parallel version






test_func<-function(n,loc,scale){
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  decision<-abs(stat)>1.96
  return(list("decision"=decision))
}

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
erg<-MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)

# test <- bench::mark({MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)},
#             {future_mc(func = test_func, repetitions = 250, param_list = param_list)}, check = FALSE)

summary(erg)



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



# summary functions -> list of function for each result in list (-> single function --> used for all, default --> summary())
# What to do with the furrr.options?, seed argument user-defined?
# Nice output -> look in tidyverse package



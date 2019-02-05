################################################
# Sources.R - this script load several scripts related with the kernel. 
################################################

cat("\t\t\t- Loading sources...\n")

 #System
 source("sources/system/fnc_read_instructions.R")
 source("sources/system/fnc_read_data.R")

 #Population
 source("sources/initialisation/fnc_ini_population.R")
 source("sources/operations/fnc_pop_evaluation.R")
 source("sources/operations/fnc_crossover.R")
 source("sources/operations/fnc_mutation.R")
 source("sources/operations/fnc_pop_selection.R")
 source("sources/operations/fnc_rand_population.R")



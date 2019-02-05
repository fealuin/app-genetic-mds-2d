main = function(carpeta)
{
  #Initial configuration
  #setwd(paste(getwd(),"/",carpeta,sep=""))
  
  #Packages
  source("sources/system/packages.R")
  
  #Sources
  source("sources/system/sources.R")
  
  #Read file with instructions
  instructions=fnc_read_instructions()
  
  #Read input data set
  data=fnc_read_data(instructions)
  
  #Initialisation of the population
  population=fnc_ini_population(instructions,data)
  
  for (a in 1:instructions$application$ending)
  { #Evaluation of the population
    population=fnc_pop_evaluation(instructions,population,data)
    print(a)
    print(c(max(population$fitness),mean(population$fitness),min(population$fitness)))
    
    #Genetic operations: crossover
    pop_crossover=fnc_crossover(instructions,population,data)
    
    #Genetic operations: mutation
    pop_mutation=fnc_mutation(instructions,population,data)
    
    #Genetic operations: random solutions
    pop_rand=fnc_rand_population(instructions,population,data)
    
    
    #Union
    population=fnc_pop_selection(instructions,population,pop_crossover,pop_mutation,pop_rand)
  }
  
  return(population)
}
#/////////////////////////////////////////////////////////////////
#
# fnc_pop_selection: script which select the population
#
#/////////////////////////////////////////////////////////////////
fnc_pop_selection=function(instructions,population,pop_crossover,pop_mutation,pop_rand){

    population$variable=c(population$variable,pop_crossover$variable,pop_mutation$variable,pop_rand$variable)
    population$fitness=c(population$fitness,pop_crossover$fitness,pop_mutation$fitness,pop_rand$fitness)
    population$mean_fitness=c(population$mean_fitness,pop_crossover$mean_fitness,pop_mutation$mean_fitness,pop_rand$mean_fitness)
    population$methods=c(population$methods,pop_crossover$methods,pop_mutation$methods,pop_rand$methods)
    
    #sorting the population
    cat("\t\t\t- Selecting the population...\n")
    population$variable=population$variable[order(as.numeric(population$fitness),decreasing = T)][1:instructions$population$size]
    population$methods=population$methods[order(as.numeric(population$fitness),decreasing = T)][1:instructions$population$size]
    population$fitness=population$fitness[order(as.numeric(population$fitness),decreasing = T)][1:instructions$population$size]
    population$mean_fitness=round(population$mean_fitness[order(as.numeric(population$fitness),decreasing = T)][1:instructions$population$size],3)
    
    return(population)
    
  }
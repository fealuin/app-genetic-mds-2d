#-----------------------------------------------------
#Packages
#-----------------------------------------------------
library("yaml")
source("backup/main.R")
library("dunn.test")
source("tunning_scripts/fnc_alg_evaluation.R")
source("tunning_scripts/fnc_statistic_filter.R")
source("tunning_scripts/fnc_create_pop.R")
 
#-----------------------------------------------------
#Definition of input paramenters
#-----------------------------------------------------
n_eval=2           #Number of evaluation of each solution (used to the statistical test) - minimum = 2 
n_algorithms=2      #Number of individuals (algorithms) that must be evaluated in each generation
n_generations=1     #Number of generations allowed to evaluate the algorithm
max_gen=2
#-----------------------------------------------------
#Definition of the algorithm paramenters
#-----------------------------------------------------
input=NULL
input$gen=sample(max_gen,n_algorithms,replace=T)      #Number of the generations of the algorithm
input$pop_size=sample(10:100,n_algorithms)
input$cross_rate=round(runif(n_algorithms,0.1,0.4),2)
input$mut_rate=round(runif(n_algorithms,0.1,0.4),2)
input$met_ini=sample(c("RAND","GPS","KNN","OWNN","PAM","SNN","RF"),n_algorithms,replace=T)

#Read the yml file
instructions=read_yaml("backup/instructions.yml")
 
 
#-------------------------
# Algorithm evaluation
#-------------------------
algorithms=NULL

#Algorithm original (Elite inicial)
print("Poblacion inicial")

algorithms=fnc_alg_evaluation(main,instructions,input,n_algorithms)

for (b in 1:n_generations)
{ 
  print(paste("Generación:",b," de ",n_generations))
   
  #Create a new set of solutions
  input2=fnc_create_pop(algorithms,max_gen)
  algorithms2=fnc_alg_evaluation(main,instructions,input2,n_algorithms)
  
  #Statistical test
  
  algorithms=fnc_statistic_filter(algorithms,algorithms2)
  
  print(algorithms$fitmean)
} 
 
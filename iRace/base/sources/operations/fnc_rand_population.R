#/////////////////////////////////////////////////////////////////
#
# fnc_pop_evaluation: script which create a random population
#
#/////////////////////////////////////////////////////////////////
fnc_rand_population=function(instructions,population,data){
  
  cat("\t\t\t- Creating a random population \n")
  

    #Input
    cross_rate=instructions$genetic_operations$crossover$rate
    mut_rate=instructions$genetic_operations$mutation$rate
    ncores=instructions$application$cores
    pop_size=instructions$population$size
    threshold_cross=floor(pop_size*cross_rate)
    index_method=instructions$population$evaluation$fitness_fnc
    threshold_mut=floor(pop_size*mut_rate)
    prob_input=1-instructions$population$initialization$probability
    threshold_rand=pop_size-threshold_cross-threshold_mut
    
    #Function to evaluate the random population
    fnc_create_random_pop_local=function(n)
    { 
      distance_method=instructions$population$evaluation$distance
      
      variable=sample(0:1,(ncol(data)-1),replace = T,prob = c(prob_input,(1-prob_input)))
      d1=data[,which(variable==1)]
      #Distance matrices
      #depending on distance methods
      if (distance_method=="EUC"){mat_d1=as.matrix(dist(d1))}
      if (distance_method=="MAN"){mat_d1=as.matrix(dist(d1,method="manhattan"))}
      if (distance_method=="SPE"){mat_d1=(as.matrix(Dist(d1, method = "spearman")))}
      if (distance_method=="CEN"){mat_d1=(as.matrix(Dist(d1, method = "correlation")))}
      
      options(warn=-1)
      #KMEANS
      set.seed(1)
      c1_kme=kmeans(mat_d1,5)$cluster
      #PAM
      set.seed(1)
      c1_pam=pam(mat_d1,5)$cluster
      
      #CLARA
      set.seed(1)
      c1_cla=clara(mat_d1,5)$cluster
      
      #FANNY
      set.seed(1)
      c1_fan=fanny(mat_d1,5)$cluster
      
      set.seed(NULL)
      q1_kme=comPart(data$class, c1_kme,type=index_method)
      q1_pam=comPart(data$class, c1_pam,type=index_method)
      q1_cla=comPart(data$class, c1_cla,type=index_method)
      q1_fan=comPart(data$class, c1_fan,type=index_method)
      quality=c(q1_kme,q1_pam,q1_cla,q1_fan)
      
      output=NULL
      output$variable=variable
      output$methods=c("KME","CLA","PAM","FAN")[which.max(quality)]
      output$fitness=round(quality[which.max(quality)],3)
      output$mean_fitness=mean(quality)
      
      return(output)
    } #End local function
    
    output_tmp=NULL
    output_tmp=mclapply(1:threshold_rand,fnc_create_random_pop_local,mc.cores = ncores)
    
    output=NULL
    #Sorting the output
    for (a in 1:length(output_tmp))
    { output$variable[[a]]=output_tmp[[a]]$variable
      output$methods[[a]]=output_tmp[[a]]$methods
      output$fitness[[a]]=output_tmp[[a]]$fitness
      output$mean_fitness[[a]]=output_tmp[[a]]$mean_fitness
    }
    
    #sorting the population
    cat("\t\t\t- Sorting the population...\n")
    output$variable=output$variable[order(as.numeric(output$fitness),decreasing = T)][1:length(output$variable)]
    output$methods=output$methods[order(as.numeric(output$fitness),decreasing = T)][1:length(output$variable)]
    output$mean_fitness=round(output$mean_fitness[order(as.numeric(output$fitness),decreasing = T)][1:length(output$variable)],3)
    output$fitness=round(output$fitness[order(as.numeric(output$fitness),decreasing = T)][1:length(output$variable)],3)
    
    #Return
    return(output)
    
}#End fuction
    
    

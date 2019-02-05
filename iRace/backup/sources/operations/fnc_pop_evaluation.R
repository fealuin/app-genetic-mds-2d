#/////////////////////////////////////////////////////////////////
#
# fnc_pop_evaluation: script which evaluate the input population
#
#/////////////////////////////////////////////////////////////////
fnc_pop_evaluation=function(instructions,population,data){
  
  cat("\t\t\t- Evaluation of the population...\n")
  
    #Inputs
    distance_method=instructions$population$distance
    pop_size=instructions$population$size
    ncores=instructions$application$cores
    distance_method = instructions$population$evaluation$distance
    index_method=instructions$population$evaluation$fitness_fnc
    
    fnc_evaluate_pop_local=function(n)
    {
      
      if (population$fitness[[n]]=="NA"){
        
        partial_data=data[,which(population$variable[[n]]==1)]
      
        
        #depending on distance methods
        if (distance_method=="EUC"){mat_dist=as.matrix(dist(partial_data))}
        if (distance_method=="MAN"){mat_dist=as.matrix(dist(partial_data,method="manhattan"))}
        if (distance_method=="SPE"){mat_dist=(as.matrix(Dist(partial_data, method = "spearman")))}
        if (distance_method=="CEN"){mat_dist=(as.matrix(Dist(partial_data, method = "correlation")))}
          
        #Application of the different strategies of cluster
        options(warn=-1)
        set.seed(1)
        cluster_kme=kmeans(mat_dist,5)$cluster #Kmeans
        set.seed(1)
        cluster_cla=clara(mat_dist,5)$cluster  #CLARA
        set.seed(1)
        cluster_pam=pam(mat_dist,5)$cluster    #PAM
        set.seed(1)
        cluster_fan=fanny(mat_dist,5)$cluster  #FANNY
        options(warn=-0)
        set.seed(NULL)
        
        #Index calculartion
        quality_kme=comPart(data$class, cluster_kme,type=index_method)
        quality_cla=comPart(data$class, cluster_cla,type=index_method)
        quality_pam=comPart(data$class, cluster_pam,type=index_method)
        quality_fan=comPart(data$class, cluster_fan,type=index_method)
        quality=c(quality_kme,quality_cla,quality_pam,quality_fan)
        
        output=NULL
        output$variable=population$variable[[n]]
        output$methods=c("KME","CLA","PAM","FAN")[which.max(quality)]
        output$fitness=round(quality[which.max(quality)],3)
        output$mean_fitness=mean(quality)
        
      } else {
        output=NULL
        output$variable=population$variable[[n]]
        output$methods=population$methods[[n]]
        output$fitness=population$fitness[[n]]
        output$mean_fitness=population$mean_fitness[[n]]
      }
      
      return(output)
    } #end function
      
    output_tmp=mclapply(1:pop_size,fnc_evaluate_pop_local,mc.cores = ncores)
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
    output$variable=output$variable[order(as.numeric(output$fitness),decreasing = T)][1:instructions$population$size]
    output$methods=output$methods[order(as.numeric(output$fitness),decreasing = T)][1:instructions$population$size]
    output$mean_fitness=round(output$mean_fitness[order(as.numeric(output$fitness),decreasing = T)][1:instructions$population$size],3)
    output$fitness=round(output$fitness[order(as.numeric(output$fitness),decreasing = T)][1:instructions$population$size],3)
    return(output)
} #End function

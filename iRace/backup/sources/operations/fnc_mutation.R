#/////////////////////////////////////////////////////////////////
#
# fnc_mutation: script which performs the crossover operation
#
#/////////////////////////////////////////////////////////////////
fnc_mutation=function(instructions,population,data){
  
  cat("\t\t\t- Appliying mutation operator...\n")
  
  #Input
  mut_rate=instructions$genetic_operations$mutation$rate
  ncores=instructions$application$cores
  pop_size=instructions$population$size
  threshold_mut=floor(pop_size*mut_rate)
  index_method=instructions$population$evaluation$fitness_fnc
  prob_input=1-instructions$population$initialization$probability

    fnc_mutation_local=function(n)
    { 
  
      #Input
      pop_size=instructions$population$size
      distance_method=instructions$population$evaluation$distance
      
      #Roulette tournament
      roulette=c(0,cumsum(as.numeric(population$fitness)/sum(as.numeric(population$fitness)))[1:(pop_size-1)])
      selection1=findInterval(runif(pop_size,0,1),roulette)
      
      sel1=population$variable[[selection1[n]]]
      masc=sample(0:1,length(sel1),replace=T,prob=c(prob_input,(1-prob_input)))
      sel1[which(masc==1)]=abs(1-sel1[which(masc==1)])
      sol1=sel1
      sol2=abs(1-sel1)
      
      d1=data[,which(sol1==1)]
      d2=data[,which(sol2==1)]
      
      #Distance matrices
      #depending on distance methods
      if (distance_method=="EUC")
      {mat_d1=as.matrix(dist(d1))
      mat_d2=as.matrix(dist(d2))
      }
      if (distance_method=="MAN")
      {mat_d1=as.matrix(dist(d1,method="manhattan"))
      mat_d2=as.matrix(dist(d2,method="manhattan"))
      }
      if (distance_method=="SPE"){
        #Compute the distance
        mat_d1=as.matrix(Dist(d1, method = "spearman"))
        mat_d2=as.matrix(Dist(d1, method = "spearman"))
      }
      if (distance_method=="CEN"){
        #Compute the distance
        mat_d1=as.matrix(Dist(d1, method = "correlation"))
        mat_d2=as.matrix(Dist(d1, method = "correlation"))
      }
      
      options(warn=-1)
      #KMEANS
      set.seed(1)
      c1_kme=kmeans(mat_d1,5)$cluster
      set.seed(1)
      c2_kme=kmeans(mat_d2,5)$cluster
      
      #PAM
      set.seed(1)
      c1_pam=pam(mat_d1,5)$cluster
      set.seed(1)
      c2_pam=pam(mat_d2,5)$cluster
      
      #CLARA
      set.seed(1)
      c1_cla=clara(mat_d1,5)$cluster
      set.seed(1)
      c2_cla=clara(mat_d2,5)$cluster
      
      #FANNY
      set.seed(1)
      c1_fan=fanny(mat_d1,5)$cluster
      set.seed(1)
      c2_fan=fanny(mat_d2,5)$cluster
      options(warn=0)
      set.seed(NULL)
      
      q1_kme=comPart(data$class, c1_kme,type=index_method)
      q2_kme=comPart(data$class, c2_kme,type=index_method)
      q1_pam=comPart(data$class, c1_pam,type=index_method)
      q2_pam=comPart(data$class, c2_pam,type=index_method)
      q1_cla=comPart(data$class, c1_cla,type=index_method)
      q2_cla=comPart(data$class, c2_cla,type=index_method)
      q1_fan=comPart(data$class, c1_fan,type=index_method)
      q2_fan=comPart(data$class, c2_fan,type=index_method)
      
      
      quality=round(c(q1_kme,q2_kme,q1_pam,q2_pam,q1_cla,q2_cla,q1_fan,q2_fan),3)
      output=NULL
      output$fitness=max(quality)[1]
      output$methods=c("KME","KME","PAM","PAM","CLA","CLA","FAN","FAN")[which.max(quality)[1]]
      mod_tmp=(which.max(quality)[1])%%2
      if (mod_tmp==1)
      {output$variable=sol1
        output$mean_fitness=mean(quality[1],quality[3],quality[5],quality[7])
      }
      if (mod_tmp==0) 
      {output$variable=sol2
        output$mean_fitness=mean(quality[2],quality[4],quality[6],quality[8])
      }
      
      return(output)
    } #end local function
    
  
  output_tmp=mclapply(1:threshold_mut,fnc_mutation_local,mc.cores = ncores)
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
  output$variable=output$variable[order(as.numeric(output$fitness),decreasing = T)][1:threshold_mut]
  output$methods=output$methods[order(as.numeric(output$fitness),decreasing = T)][1:threshold_mut]
  output$mean_fitness=round(output$mean_fitness[order(as.numeric(output$fitness),decreasing = T)][1:threshold_mut],3)
  output$fitness=round(output$fitness[order(as.numeric(output$fitness),decreasing = T)][1:threshold_mut],3)
  
  return(output)
}#End function
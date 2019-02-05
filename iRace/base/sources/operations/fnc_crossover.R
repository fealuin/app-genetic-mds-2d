#/////////////////////////////////////////////////////////////////
#
# fnc_crossover: script which performs the crossover operation
#
#/////////////////////////////////////////////////////////////////
fnc_crossover=function(instructions,population,data){
  
  cat("\t\t\t- Appliying crossover operator...\n")

  #Input
  cross_rate=instructions$genetic_operations$crossover$rate
  ncores=instructions$application$cores
  pop_size=instructions$population$size
  threshold_cross=floor(pop_size*cross_rate)
  index_method=instructions$population$evaluation$fitness_fnc
  
  
      fnc_crossover_local=function(n)
      { 
        #Input
        pop_size=instructions$population$size
        distance_method=instructions$population$evaluation$distance
        
        #Roulette tournament
        pop_size=instructions$population$size
        roulette=c(0,cumsum(as.numeric(population$fitness)/sum(as.numeric(population$fitness)))[1:(pop_size-1)])
        selection1=findInterval(runif(pop_size,0,1),roulette)
        selection2=findInterval(runif(pop_size,0,1),roulette)
        
      
        sel1=population$variable[[selection1[n]]]
        sel2=population$variable[[selection2[n]]]
      
        masc1=sample(0:1,length(sel2),replace = T)
        masc2=1-masc1
        sol1=masc1*sel1+masc2*sel2
        sol2=1-sol1
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
          mat_d1=(as.matrix(Dist(d1, method = "spearman")))
          mat_d2=(as.matrix(Dist(d1, method = "spearman")))
          }
        if (distance_method=="CEN"){
          #Compute the distance
          mat_d1=(as.matrix(Dist(d1, method = "correlation")))
          mat_d2=(as.matrix(Dist(d1, method = "correlation")))
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
      
      
      output_tmp=mclapply(1:threshold_cross,fnc_crossover_local,mc.cores = ncores)
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
      output$variable=output$variable[order(as.numeric(output$fitness),decreasing = T)][1:threshold_cross]
      output$methods=output$methods[order(as.numeric(output$fitness),decreasing = T)][1:threshold_cross]
      output$mean_fitness=round(output$mean_fitness[order(as.numeric(output$fitness),decreasing = T)][1:threshold_cross],3)
      output$fitness=round(output$fitness[order(as.numeric(output$fitness),decreasing = T)][1:threshold_cross],3)
      
  return(output)
}#End function
      
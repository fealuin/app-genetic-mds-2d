#/////////////////////////////////////////////////////////////////
#
# fnc_ini_population: script which initialise the population
#
#/////////////////////////////////////////////////////////////////
fnc_ini_population=function(instructions,data)
{
  cat("----------------------------------------------------------------------------------------\n")
  cat("\t\t B)\tInitial population\n")
  cat("----------------------------------------------------------------------------------------\n")
  options(warn=-1)
  
  #Define inputs
  pop_size=instructions$population$size
  ncores=instructions$application$cores
  ini_method=instructions$population$initialization$method
  prob_input=1-instructions$population$initialization$probability
  
  #Function to create a random population
  fnc_create_random_pop=function(n)
  { return(sample(0:1,(ncol(data)-1),replace = T,prob = c(prob_input,(1-prob_input))))}
  
  #.....................................
  #A) Random initialization
  #.....................................
  if (ini_method=="RAND")
  { 
    cat("\t\t\t- Random initialization of population...\n")
    pop_ini=NULL
    pop_ini$variable=mclapply(1:pop_size,fnc_create_random_pop,mc.cores = ncores)
    pop_ini$variable[[1]]=pop_ini$variable[[1]]*0+1
  }
  
  
  #.....................................
  #C) Other strategies
  #GPS = Greedy Prototype Selection
  #KNN = k-Nearest Neighbors
  #PAM = Nearest Shrunken Centroids
  #OWNN = Optimal Weighted Nearest Neighbor Classifier
  #SNN = Stabilized Nearest Neighbor Classifier
  #.....................................
  if ((ini_method=="GPS") | (ini_method=="KNN") | (ini_method=="PAM") | (ini_method=="OWNN") | (ini_method=="SNN")) 
  {
    
    if (ini_method=="GPS") {local_method="protoclass"}
    if (ini_method=="KNN") {local_method="knn"}
    if (ini_method=="PAM") {local_method="pam"}
    if (ini_method=="OWNN") {local_method="ownn"}
    if (ini_method=="SNN") {local_method="snn"}
    
    cat(paste("\t\t\t- Initialization of population based on ",ini_method," method...\n",sep=""))
    
    # prepare training scheme
    control <- trainControl(method="repeatedcv", number=10, repeats=1,verboseIter = F)
    
    pop_ini=NULL
    #Saving the original classes
    class_backup=data$class
    for (a in 1:max(as.numeric(data$class)))
    { 
      cat(paste("\t\t\t\t- Creating the individual N-",a,"\n",sep=""))
      
      class_tmp=as.numeric(data$class)
      class_tmp[which(class_tmp!=a)]=0
      
      data$class=as.factor(class_tmp)
      
      # train the model
      model = caret::train(class~., data=data, method=local_method, preProcess="scale", trControl=control)
      
      # estimate variable importance
      importance <- varImp(model, scale=FALSE)
      
      rownames(importance$importance)=rownames(importance$importance)[order(importance$importance[,2],decreasing = T)]
      importance$importance[,2]=importance$importance[,2][order(importance$importance[,2],decreasing = T)]
      indexes=which(importance$importance[,2]>quantile(importance$importance[,2])[3])
      g1=rownames(importance$importance)[indexes] #Mayores al percentil 75
      pop_ini$variable[[a]]=as.numeric(matrix(0,1,(ncol(data)-1)))
      pop_ini$variable[[a]][match(g1,colnames(data))]=1
      
      if (sum(pop_ini$variable[[a]])==0){pop_ini$variable[[a]][1:5]=1}
      
      #Return the original class
      data$class=class_backup
    }
    pop_ini2=NULL
    pop_ini2$variable=mclapply(1:(pop_size-5),fnc_create_random_pop,mc.cores = ncores)
    pop_ini$variable=c(pop_ini$variable,pop_ini2$variable)
  }
  
  if (ini_method=="RF")
  { 
    cat("\t\t\t- Initialization of population based on Random forest...\n")
    
    # prepare training scheme
    control <- trainControl(method="repeatedcv", number=10, repeats=1,verboseIter = F)
    
    # train the model
    model = caret::train(class~., data=data, method="rf", preProcess="scale", trControl=control)
    
    # estimate variable importance
    importance <- varImp(model, scale=FALSE)
    
    #Clase 1
    pop_ini=NULL
    rownames(importance$importance)=rownames(importance$importance)[order(importance$importance[,1],decreasing = T)]
    importance$importance[,1]=importance$importance[,1][order(importance$importance[,1],decreasing = T)]
    indexes=which(importance$importance[,1]>quantile(importance$importance[,1])[3])
    g1=rownames(importance$importance)[indexes] #Mayores al percentil 75
    pop_ini$variable[[1]]=as.numeric(matrix(0,1,(ncol(data)-1)))
    pop_ini$variable[[1]][match(g1,colnames(data))]=1
    pop_ini2=NULL
    pop_ini2$variable=mclapply(1:(pop_size-1),fnc_create_random_pop,mc.cores = ncores)
    pop_ini$variable=c(pop_ini$variable,pop_ini2$variable)
  }
  
  pop_ini$variable[[pop_size]]=pop_ini$variable[[pop_size]]*0+1
  
  #Add cluster strategy
  pop_ini$methods=rep("NA",pop_size)
  #Add fitness value
  pop_ini$fitness=rep("NA",pop_size)
  #Add mean fitness value
  pop_ini$mean_fitness=rep("NA",pop_size)
  
  options(warn=0)
  return(pop_ini)
}
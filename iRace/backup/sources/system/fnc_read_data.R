#/////////////////////////////////////////////////////////////////
#
# fnc_read_data(): script which read the data
#
#/////////////////////////////////////////////////////////////////

fnc_read_data=function(instructions)
{ 
  cat("\t\t\t- Reading data...\n")
  
  #Reading of data
  data_tmp=read.csv(paste("data/",instructions$input$name_data,sep=""),header=T,sep=";")
  
  cat("\t\t\t- Filtering data according variance...\n")
  data_tmp=data_tmp[,nearZeroVar(data_tmp,freqCut = 19,uniqueCut = 10)]            #This operation removes the name of the hospitals
  
  cat("\t\t\t- Adding the classes...\n")
  #Reading of class
  class_tmp=read.csv(paste("data/",instructions$input$name_cluster,sep=""),header=F,sep=";")
  
  #Define class
  data_tmp=cbind(data_tmp,class_tmp[,2])    #Assumes that the files have the hospitals sorted in the same way.
  colnames(data_tmp)[ncol(data_tmp)]="class"
  data_tmp$class=as.factor(data_tmp$class)
  
  #Remove the name of the hospital
  #data_tmp=data_tmp[,-1]
  
  cat("\t\t\t- Filtering data according to chi-squares...\n") 
  indexes = chi.squared(class~., data_tmp)
  indexes = which(indexes>0)
  data_tmp = data_tmp[,indexes]
  
  #Define class
  data_tmp=cbind(data_tmp,class_tmp[,2])    #Assumes that the files have the hospitals sorted in the same way.
  colnames(data_tmp)[ncol(data_tmp)]="class"
  data_tmp$class=as.factor(data_tmp$class)
  
  cat("\t\t\t- Applying feature scale normalisation...\n")
  #Normalization
  for (a in 1:(ncol(data_tmp)-1))
  {data_tmp[,a]=(data_tmp[,a]-min(data_tmp[,a]))/(max(data_tmp[,a])-min(data_tmp[,a]))}
  

  #Balance the data
   if (instructions$population$initialization$balance=="T")
   {
     cat("\t\t\t- Balancing the data set...\n")
     input=data_tmp[,1:(ncol(data_tmp)-1)]
     output=data_tmp$class
     data_adj=ubSMOTE(X=input, Y= output,perc.over = 100)
     
     data_tmp=cbind(data_adj$X,class=data_adj$Y)
   }
  return(data_tmp) 
}
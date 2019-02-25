library(mco)
library(nsga2R)
library(emoa)
#library(ggplot2)
library(ecr)

source('./population/population.r')
geneticMds2<-function(D1,D2,gen,size,m=2,initMethod='cmdscaleMean',radio,mutationRatio=runif(1),crossOverRatio=runif(1)){
  #data=list()
  pop=Population(D1,D2,size)
  pop$initialize(type=initMethod)
  pop$setFitness()
  data<-data.frame(cbind(pop$getOrderByFitness(),1))
  if(gen>1){
    for( i in 2:gen){
      pop2=pop$getCrossOver(ratio = crossOverRatio)
      pop2$setMutation(radio=radio,p=mutationRatio)
      pop2$setFitness()
      pop$setIndividuals(append(pop$getIndividuals(),pop2$getIndividuals()))
      pop$orderByFitness()
      pop$setIndividuals(pop$getIndividuals()[1:size])
      #data order
      data<-rbind(data,data.frame(cbind(pop$getOrderByFitness(),as.numeric(i))))
    }
  }
  #get data (points)
  res=list(data,pop$getData())
  return(res)
}

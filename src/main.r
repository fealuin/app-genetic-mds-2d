library(mco)
library(nsga2R)
library(emoa)
#library(ggplot2)
library(ecr)
library(svMisc)

source('./population/population.r')
geneticMds2<-function(D1,D2,gen=458,size=345,m=2,initMethod='cmdscaleMean',radio=0.6,mutationRatio=0.9,crossOverRatio=0.8){
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
      progress(i,gen)
    }
  }
  #get data (points)
  res=list(data,pop$getData())
  return(res)
}

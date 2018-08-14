library(mlbench)
library(parallel)
library(RMySQL)


source('./main.r')
#DB connection
con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')

set.seed(1)
#Data
data(Glass)
data(PimaIndiansDiabetes)
data(iris)

dataGlass<-Glass[,1:9]
dataDiabetes<-PimaIndiansDiabetes[,1:8]
dataIris<-iris[,1:4]

D1=dist(dataGlass)
D2=as.dist(cor(t(dataGlass)))

D3=dist(dataIris)
D4=as.dist(cor(t(dataIris)))

D5=dist(dataDiabetes)
D6=as.dist(cor(t(dataDiabetes)))

#Parameters
dataset=c('glass','iris','diabetes')
init=c('cmdscaleMean','cmdscalePca')
runs=c(1:11)
radio=seq(0.1,1,by=0.1)
popSize=c(10,50,100)
gen=100
parameters=expand.grid(dataset,init,runs,radio,popSize,gen)
names(parameters)=c('dataset','initialization','runs','radio','population_size','generations')
parameters$id=seq.int(nrow(parameters))
#save parameters
dbWriteTable(con,'parameters',parameters,row.name=FALSE,overwrite=TRUE)
#Clear results table
dbSendQuery(con,'truncate table results')
dbSendQuery(con,'truncate table individuals')
apply(
  parameters,
  1,
  function(param){
    if(param['dataset']=='glass'){
      D01<-D1
      D02<-D2
    }
    if(param['dataset']=='iris'){
      D01<-D3
      D02<-D4
    }
    if(param['dataset']=='diabetes'){
      D01<-D5
      D02<-D6
    }
    #Results(orderByFitness)
    res<-geneticMds2(D01,D02,gen=as.numeric(param['generations']),size=param['population_size'],m=2,initMethod=param['initialization'],radio=as.numeric(param['radio']))
    res[[1]]<-cbind(param['id'],seq.int(nrow(res[[1]])),res[[1]],param['runs'])
    names(res[[1]])<-c('parameters_id','individual_id','x','y','rank','crowding','generation','run')
    dbWriteTable(con,'results',res[[1]],row.name=FALSE,append=TRUE)
    #individuals (points)
    res[[2]]=cbind(param['id'],param['runs'],res[[2]])
    names(res[[2]])=c('parameters_id','run','individual_id','point_id','x','y')
    dbWriteTable(con,'individuals',res[[2]],row.name=FALSE,append=TRUE)

  }
)
dbDisconnect(con)

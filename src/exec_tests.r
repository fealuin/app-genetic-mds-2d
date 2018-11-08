library(mlbench)
library(parallel)
library(RMySQL)
library(treespace)
library(phangorn)



source('./main.r')
#DB connection
con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')
set.seed(1)
abalone<-read.csv('../datasets/abalone.data',header = FALSE)
abalone<-abalone[sample(nrow(abalone),1000),]
abaloneDist<-dist(abalone[,-1])
abaloneCor<-as.dist(cor(t(abalone[,-1])))

breast_cancer_wisconsin<-read.csv('../datasets/breast-cancer-wisconsin.data',header = FALSE)
breast_cancer_wisconsin<-sapply(breast_cancer_wisconsin,as.integer)
bcwDist<-dist(breast_cancer_wisconsin[,-1])
bcwCor<-as.dist(cor(t(breast_cancer_wisconsin[,-1])))

ionosphere<-read.csv('../datasets/ionosphere.data',header = FALSE)
ionosphereDist<-dist(ionosphere[,-35])
ionosphereCor<-as.dist(cor(t(ionosphere[,-35])))

fluTreesRF=RF.dist(fluTrees)
fluTreesKF=KF.dist(fluTrees)

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
dataset=c('glass','iris','diabetes','abalone','bcw','ionosphere','fluTrees')
init=c('cmdscaleMean')
runs=c(1:11)
radio=0.5
popSize=c(100)
gen=160
parameters=expand.grid(dataset,init,runs,radio,popSize,gen)
names(parameters)=c('dataset','initialization','runs','radio','population_size','generations')
parameters$id=seq.int(nrow(parameters))
#save parameters
dbWriteTable(con,'parameters_tests',parameters,row.name=FALSE,overwrite=TRUE)
#Clear results table
try({
dbSendQuery(con,'truncate table results_tests')
dbSendQuery(con,'truncate table individuals_tests')
})
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
    if(param['dataset']=='abalone'){
      D01<-abaloneDist
      D02<-abaloneCor
    }
    if(param['dataset']=='bcw'){
      D01<-bcwDist
      D02<-bcwCor
    }
    if(param['dataset']=='ionosphere'){
      D01<-ionosphereDist
      D02<-ionosphereCor
    }
    if(param['dataset']=='fluTrees'){
      D01<-fluTreesKF
      D02<-fluTreesRF
    }
    #Results(orderByFitness)
    res<-geneticMds2(D01,D02,gen=as.numeric(param['generations']),size=param['population_size'],m=2,initMethod=param['initialization'],radio=as.numeric(param['radio']))
    res[[1]]<-cbind(param['id'],seq.int(nrow(res[[1]])),res[[1]],param['runs'])
    names(res[[1]])<-c('parameters_id','individual_id','x','y','rank','crowding','generation','run')
    dbWriteTable(con,'results_tests',res[[1]],row.name=FALSE,append=TRUE)
    #individuals (points)
    res[[2]]=cbind(param['id'],param['runs'],res[[2]])
    names(res[[2]])=c('parameters_id','run','individual_id','point_id','x','y')
    dbWriteTable(con,'individuals_tests',res[[2]],row.name=FALSE,append=TRUE)

  }
)
dbDisconnect(con)

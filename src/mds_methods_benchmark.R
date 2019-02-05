library(vegan)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
#library(usedist)
library(mlbench)
library(parallel)
library(RMySQL)
library(treespace)
library(phangorn)

source("./main.r")

##Puntos
#cmdscale(M,k=2)
#vegan::wcmdscale(M, k=2, w=rep(1,ncol(M)))
#labdsv::pco(b)$points
#ape::pcoa(M)$vectors
#ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li
#smacofSym(M,ndim=2)$conf

getstresses <- function(D,points){
    D1=dist(points)
    raw<-sum((D-D1)^2)
    stress<-raw/sum(D^2)
    mse<-raw/(ncol(as.matrix(D))^2)
    normalized<-sqrt(stress)
    sstress<-sqrt(sum((D^2-D1^2)^2)/sum((D^2)^2))
    return(unname(c(raw,stress,mse,normalized,sstress)))
}

getstress <- function(D,points){
  D1=dist(points)
  raw<-sum((D-D1)^2)
  stress<-raw/sum(D^2)
  return(stress)
}


mdsrun <- function(D1,D2){
   
    #res<-data.frame()
    res<-list()
    res[[1]]<-list()
    res[[2]]<-list()
    res[[3]]<-list()
    res[[4]]<-list()
     cmdscaleTime<-system.time(cmdscaleDist<-cmdscale(D1,k=2))[2:3]
    veganTime<- system.time(veganDist<-vegan::wcmdscale(D1, k=2))[2:3]
    ecodistTime<-system.time(ecodistDist<-ecodist::pco(D1)$vectors[,1:2])[2:3]
    labdsvTime<-system.time(labdsvDist<-labdsv::pco(D1)$points)[2:3]
    #apeTime<-system.time(apeDist<-ape::pcoa(D)$vectors)[2:3]
    #ade4Time<-system.time(ade4Dist<-ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li)[2:3]
    smacofTime<-system.time(smacofDist<-smacof::smacofSym(D1,ndim=2)$conf)[2:3]
    
    res[[1]][['cmdscale']]<-cmdscaleDist
    res[[1]][['vegan']]<-veganDist
    res[[1]][['ecodist']]<-ecodistDist
    res[[1]][['labdsv']]<-labdsvDist
    res[[1]][['smacof']]<-smacofDist
    
    #res[['ape']]<-apeDist
    #res[['ade4']]<-ade4Dist
    
    cmdscaleTime<-system.time(cmdscaleDist<-cmdscale(D2,k=2))[2:3]
    veganTime<- system.time(veganDist<-vegan::wcmdscale(D2, k=2))[2:3]
    ecodistTime<-system.time(ecodistDist<-ecodist::pco(D2)$vectors[,1:2])[2:3]
    labdsvTime<-system.time(labdsvDist<-labdsv::pco(D2)$points)[2:3]
    #apeTime<-system.time(apeDist<-ape::pcoa(D)$vectors)[2:3]
    #ade4Time<-system.time(ade4Dist<-ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li)[2:3]
    smacofTime<-system.time(smacofDist<-smacof::smacofSym(D2,ndim=2)$conf)[2:3]
    
    res[[2]][['cmdscale']]<-cmdscaleDist
    res[[2]][['vegan']]<-veganDist
    res[[2]][['ecodist']]<-ecodistDist
    res[[2]][['labdsv']]<-labdsvDist
    res[[2]][['smacof']]<-smacofDist
    
    res[[3]][['cmdscale']]<-c(getstress(D1,res[[1]][['cmdscale']]),getstress(D2,res[[1]][['cmdscale']]))
    res[[3]][['vegan']]<-c(getstress(D1,res[[1]][['vegan']]),getstress(D2,res[[1]][['vegan']]))
    res[[3]][['ecodist']]<-c(getstress(D1,res[[1]][['ecodist']]),getstress(D2,res[[1]][['ecodist']]))
    res[[3]][['labdsv']]<-c(getstress(D1,res[[1]][['labdsv']]),getstress(D2,res[[1]][['labdsv']]))
    res[[3]][['smacof']]<-c(getstress(D1,res[[1]][['smacof']]),getstress(D2,res[[1]][['smacof']]))
    
    res[[4]][['cmdscale']]<-c(getstress(D1,res[[2]][['cmdscale']]),getstress(D2,res[[2]][['cmdscale']]))
    res[[4]][['vegan']]<-c(getstress(D1,res[[2]][['vegan']]),getstress(D2,res[[2]][['vegan']]))
    res[[4]][['ecodist']]<-c(getstress(D1,res[[2]][['ecodist']]),getstress(D2,res[[2]][['ecodist']]))
    res[[4]][['labdsv']]<-c(getstress(D1,res[[2]][['labdsv']]),getstress(D2,res[[2]][['labdsv']]))
    res[[4]][['smacof']]<-c(getstress(D1,res[[2]][['smacof']]),getstress(D2,res[[2]][['smacof']]))
    
    #return (unname(c(cmdscaleTime,cmdscaleStress,veganTime,veganStress,ecodistTime,ecodistTime,labdsvTime,labdsvStress,apeTime,apeStress,ade4Time,ade4Stress,smacofTime,smacofStress)))
    return(res)
}


set.seed(1)
abalone<-read.csv('../datasets/abalone.data',header = FALSE)
abalone<-abalone[sample(nrow(abalone),1000),]
abaloneDist<-dist(abalone[,-1])
abaloneCor<-as.dist((1-cor(t(abalone[,-1])))/2)

breast_cancer_wisconsin<-read.csv('../datasets/breast-cancer-wisconsin.data',header = FALSE)
breast_cancer_wisconsin<-sapply(breast_cancer_wisconsin,as.integer)
bcwDist<-dist(breast_cancer_wisconsin[,-1])
bcwCor<-as.dist((1-cor(t(breast_cancer_wisconsin[,-1])))/2)

ionosphere<-read.csv('../datasets/ionosphere.data',header = FALSE)
ionosphereDist<-dist(ionosphere[,-35])
ionosphereCor<-as.dist((1-cor(t(ionosphere[,-35])))/2)

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
D2=as.dist((1-cor(t(dataGlass)))/2)

D3=dist(dataIris)
D4=as.dist((1-cor(t(dataIris)))/2)

D5=dist(dataDiabetes)
D6=as.dist((1-cor(t(dataDiabetes)))/2)


#Parameters
functions=c('cmdscale','vegan','ecodist','labdsv','ape','ade4','smacof')
dataset=c('glass','iris','diabetes','abalone','bcw','ionosphere')
runs=c(1:21)
parameters=expand.grid(dataset,runs)

names(parameters)=c('dataset','run')
parameters$id=seq.int(nrow(parameters))
res<-list()

res<-apply(
  parameters,
  1,
  function(param){
    print(param['dataset'])
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
    print('Iniciando')
    mdsrun(D01,D02)
  }
  )

#Parameters

dataset=c('glass','iris','diabetes','abalone','bcw','ionosphere')
runs2=c(1)
parameters2=expand.grid(dataset,runs2)

names(parameters2)=c('dataset','runs')
parameters2$id=seq.int(nrow(parameters))
res2<-list()


res2<-apply(
  parameters2,
  1,
  function(param){
    print(param['dataset'])
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
    print('Dist')
    #mdsrun(D01)
    print('Cor')
    #mdsrun(D02)
    
    geneticMds2(D01,D02,160,100,radio=0.5)
  }
)
#plot pareto
plot(res2[[1]][[1]][res2[[1]][[1]]$X5==160,][,1:2])

#plot mds

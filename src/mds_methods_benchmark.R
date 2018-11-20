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
  raw<-sum((D-D1)^2)
  stress<-raw/sum(D^2)
  retunr(stress)
}


mdsrun <- function(D){
   
    #res<-data.frame()
    res<-list()
     cmdscaleTime<-system.time(cmdscaleDist<-cmdscale(D,k=2))[2:3]
    veganTime<- system.time(veganDist<-vegan::wcmdscale(D, k=2))[2:3]
    ecodistTime<-system.time(ecodistDist<-ecodist::pco(D)$vectors[,1:2])[2:3]
    labdsvTime<-system.time(labdsvDist<-labdsv::pco(D)$points)[2:3]
    #apeTime<-system.time(apeDist<-ape::pcoa(D)$vectors)[2:3]
    #ade4Time<-system.time(ade4Dist<-ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li)[2:3]
    smacofTime<-system.time(smacofDist<-smacof::smacofSym(D,ndim=2)$conf)[2:3]

    #cmdscaleStress<-getstress(D,cmdscaleDist)
    #veganStress<-getstress(D,veganDist)
    #ecodistStress<-getstress(D,ecodistDist)
    #labdsvStress<-getstress(D,labdsvDist)
    #apeStress<-getstress(D,apeDist)
    #ade4Stress<-getstress(D,ade4Dist)
    #smacofStress<-getstress(D,smacofDist)
    
    #print(data.frame(unname(cbind('vegan',data.frame(veganDist)))))
    
    # res<-rbind(res,unname(cbind('cmdscale',data.frame(cmdscaleDist))))
    # res<-rbind(res,unname(cbind('vegan',data.frame(veganDist)))) 
    # res<-rbind(res,unname(cbind('ecodist',data.frame(ecodistDist))))
    # res<-rbind(res,unname(cbind('labdsv',data.frame(labdsvDist))))
    # res<-rbind(res,unname(cbind('smacof',data.frame(smacofDist))))
    
    res[['cmdscale']]<-cmdscaleDist
    res[['vegan']]<-veganDist
    res[['ecodist']]<-ecodistDist
    res[['labdsv']]<-labdsvDist
    res[['smacof']]<-smacofDist
    
    #res[['ape']]<-apeDist
    #res[['ade4']]<-ade4Dist
    
    

    #return (unname(c(cmdscaleTime,cmdscaleStress,veganTime,veganStress,ecodistTime,ecodistTime,labdsvTime,labdsvStress,apeTime,apeStress,ade4Time,ade4Stress,smacofTime,smacofStress)))
    return(res)
}


set.seed(1)
abalone<-read.csv('../datasets/abalone.data',header = FALSE)
abalone<-abalone[sample(nrow(abalone),1000),]
abaloneDist<-dist(abalone[,-1])
abaloneCor<-as.dist(cor(t(abalone[,-1]))+2)

breast_cancer_wisconsin<-read.csv('../datasets/breast-cancer-wisconsin.data',header = FALSE)
breast_cancer_wisconsin<-sapply(breast_cancer_wisconsin,as.integer)
bcwDist<-dist(breast_cancer_wisconsin[,-1])
bcwCor<-as.dist(cor(t(breast_cancer_wisconsin[,-1]))+2)

ionosphere<-read.csv('../datasets/ionosphere.data',header = FALSE)
ionosphereDist<-dist(ionosphere[,-35])
ionosphereCor<-as.dist(cor(t(ionosphere[,-35]))+2)

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
D2=as.dist(cor(t(dataGlass))+2)

D3=dist(dataIris)
D4=as.dist(cor(t(dataIris))+2)

D5=dist(dataDiabetes)
D6=as.dist(cor(t(dataDiabetes))+2)


#Parameters
functions=c('cmdscale','vegan','ecodist','labdsv','ape','ade4','smacof')
dataset=c('glass','iris','diabetes','abalone','bcw','ionosphere')
runs=c(1:31)
parameters=expand.grid(dataset,runs)

names(parameters)=c('dataset','runs')
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
    print('Dist')
    mdsrun(D01)
    print('Cor')
    mdsrun(D02)
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

library(vegan)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
library(usedist)
library(mlbench)
library(parallel)
library(RMySQL)
library(treespace)
library(phangorn)
library(amap)

source('./main.R')

getstress <- function(D,points){
  D1=dist(points)
  raw<-sum((D-D1)^2)
  stress<-raw/sum(D^2)
  return(stress)
}


abalone<-read.csv('../datasets/abalone.data',header = FALSE)
abalone<-abalone[sample(nrow(abalone),1000),]
abaloneDist<-dist(abalone[,-1])
abaloneCor<-Dist(abalone[,-1],method="correlation")

breast_cancer_wisconsin<-read.csv('../datasets/breast-cancer-wisconsin.data',header = FALSE)
breast_cancer_wisconsin<-sapply(breast_cancer_wisconsin,as.integer)
bcwDist<-dist(breast_cancer_wisconsin[,-1])
bcwCor<-Dist(breast_cancer_wisconsin[,-1],method="correlation")

ionosphere<-read.csv('../datasets/ionosphere.data',header = FALSE)
ionosphereDist<-dist(ionosphere[,-35])
ionosphereCor<-Dist(ionosphere[,-35],method="correlation")

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
D2=Dist(dataGlass,method="correlation")

D3=dist(dataIris)
D4=Dist(dataIris,method="correlation")

D5=dist(dataDiabetes)
D6=Dist(dataDiabetes,method="correlation")




mds<-list()


mds[['cmdscale']]<-cmdscale(D,k=2)
mds[['vegan']]<-vegan::wcmdscale(D, k=2)
mds[['ecodist']]<-ecodist::pco(D)$vectors[,1:2]
mds[['labdsv']]<-labdsv::pco(D)$points
mds[['ape']]<-ape::pcoa(D)$vectors
mds[['ade4']]<-ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li
mds[['smacof']]<-smacof::smacofSym(D,ndim=2)$conf

lapply(mds,function(x){
  x<-as.data.frame(x)
   x<-names(x)
  }
  )







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
D2=Dist(dataGlass,method="correlation")

D3=dist(dataIris)
D4=Dist(dataIris,method="correlation")

D5=dist(dataDiabetes)
D6=Dist(dataDiabetes,method="correlation")

#Parameters
dataset=c('glass') #,'iris','diabetes','abalone','bcw','ionosphere')
runs=1:2

parameters<-expand.grid(dataset,runs)
names(parameters)<-c('dataset','run')

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
    
    res<-geneticMds2(D01,D02)

    
  }
)
dbDiscon
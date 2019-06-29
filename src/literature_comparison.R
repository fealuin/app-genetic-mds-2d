library(vegan)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
library(mlbench)
library(parallel)
library(RMySQL)
library(treespace)
library(phangorn)
library(amap)
library(plyr)
library(plotly)
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
abaloneCor<-Dist(abalone[,-1],method="manhattan")

breast_cancer_wisconsin<-read.csv('../datasets/breast-cancer-wisconsin.data',header = FALSE)
breast_cancer_wisconsin<-sapply(breast_cancer_wisconsin,as.integer)
bcwDist<-dist(breast_cancer_wisconsin[,-1])
bcwCor<-Dist(breast_cancer_wisconsin[,-1],method="manhattan")

ionosphere<-read.csv('../datasets/ionosphere.data',header = FALSE)
ionosphereDist<-dist(ionosphere[,-35])
ionosphereCor<-Dist(ionosphere[,-35],method="manhattan")

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
D2=Dist(dataGlass,method="manhattan")

D3=dist(dataIris)
D4=Dist(dataIris,method="manhattan")

D5=dist(dataDiabetes)
D6=Dist(dataDiabetes,method="manhattan")

#Parameters
dataset=c('glass')    #'iris','glass','diabetes','abalone','bcw','ionosphere','fluTrees')
runs=1:31

parameters<-expand.grid(dataset,runs)


#dataset=character(),run=numeric()

names(parameters)<-c('dataset','run')
set.seed(1)

for(i in 1:nrow(parameters)){
  print(parameters[i,]$dataset)
}
res<-list()
res.mds<-data.frame(dataset=character(),run=numeric(),method=character(),i=numeric(),x=numeric(),y=numeric(),stringsAsFactors = F)  
res.mds<-matrix(nrow=0,ncol=6)
for(i in 1:nrow(parameters)){
  param<-parameters[i,]
    if(param$dataset=='glass'){
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
    
    
    
    #stress<-res[[1]][res[[1]]$X5==max(res[[1]]$X5),]
    #mds<-res[[2]]
    #ac.mds<-data.frame(method=character(),i=numeric(),x=numeric(),y=numeric(),stringsAsFactors = F)
    ac.mds<-matrix(nrow=0,ncol=4)
    ac.mds<-rbind(ac.mds,cbind(method='cmdscale',i=1,as.matrix(cmdscale(D01,k=2))))
    ac.mds<-rbind(ac.mds,cbind(method='cmdscale',i=2,as.matrix(cmdscale(D02,k=2))))
    ac.mds<-rbind(ac.mds,cbind(method='vegan',i=1,as.matrix(vegan::wcmdscale(D01, k=2))))
    ac.mds<-rbind(ac.mds,cbind(method='vegan',i=2,as.matrix(vegan::wcmdscale(D02, k=2))))
    ac.mds<-rbind(ac.mds,cbind(method='ecodist',i=1,as.matrix(ecodist::pco(D01)$vectors[,1:2])))
    ac.mds<-rbind(ac.mds,cbind(method='ecodist',i=2,as.matrix(ecodist::pco(D02)$vectors[,1:2])))
    ac.mds<-rbind(ac.mds,cbind(method='labdsv',i=1,as.matrix(labdsv::pco(D01)$points)))
    ac.mds<-rbind(ac.mds,cbind(method='labdsv',i=2,as.matrix(labdsv::pco(D02)$points)))
    ac.mds<-rbind(ac.mds,cbind(method='ape',i=1,as.matrix(ape::pcoa(D01)$vectors[,1:2])))
    ac.mds<-rbind(ac.mds,cbind(method='ape',i=2,as.matrix(ape::pcoa(D02)$vectors[,1:2])))
    #ac.mds<-rbind(ac.mds,cbind(method='ade4',i=1,as.matrix(as.matrix(ade4::dudi.pco(D01, scannf = FALSE, nf = 2)$li))))
    #ac.mds<-rbind(ac.mds,cbind(method='ade4',i=2,as.matrix(as.matrix(ade4::dudi.pco(D02, scannf = FALSE, nf = 2)$li))))
    ac.mds<-rbind(ac.mds,cbind(method='smacof',i=1,as.matrix(smacof::smacofSym(D01,ndim=2)$conf)))
    ac.mds<-rbind(ac.mds,cbind(method='smacof',i=2,as.matrix(smacof::smacofSym(D02,ndim=2)$conf)))
    
    
      gen<-geneticMds2(D01,D02)#,size=max(runs))
      paretoId<-gen[[1]][gen[[1]]$X5==max(gen[[1]]$X5),]
      row.names(paretoId)<-NULL
      #Solo se dejan valores en la frontera de pareto..
      paretoValues<-gen[[2]][gen[[2]]$i %in% which(paretoId$X3==1),]
      ac.mds<-rbind(ac.mds,cbind(method='gen',i=paretoValues[[2]]$i,as.matrix(paretoValues[c('V3','V4')])))
    
    
    res.mds<-rbind(res.mds,cbind(dataset=as.character(param$dataset),run=param$run,ac.mds))
  }
res.mds<-as.data.frame(res.mds)
names(res.mds)<-c('dataset','run','method','i','x','y')

res.unique<-unique(res.mds[c('dataset','run','method','i')]) 
s1<-s2<-c()

for(i in 1:nrow(res.unique)){
  if(param$dataset=='glass'){
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

    s1[i]<-getstress(D01,res.mds[res.mds$dataset== res.unique[i,]$dataset & res.mds$method==res.unique[i,]$method & res.mds$i==res.unique[i,]$i   & res.mds$run==res.unique[i,]$run,c('x','y')])
    s2[i]<-getstress(D02,res.mds[res.mds$dataset== res.unique[i,]$dataset & res.mds$method==res.unique[i,]$method & res.mds$i==res.unique[i,]$i   & res.mds$run==res.unique[i,]$run,c('x','y')])

}
res.stress<-data.frame(res.unique,s1,s2)

save(res.stress,res.mds,file='d.Rdata')

#d<-'diabetes'
for(d in dataset){
p<-plot_ly(res.stress[res.stress$dataset==d,],x=~s1,y=~s2,color=~method,mode='markers',type='scatter')%>%
  layout(title=d)

htmlwidgets::saveWidget(p, file = file.path( normalizePath('./plot/cl'),paste0(d,'.html')))
  #print(ncol(res.mds[res.mds$dataset== res.unique$dataset & res.mds$method==res.unique$method & res.mds$i==res.unique$i   & res.mds$run==res.unique$run,c('x','y')]))
  #getstress(D01,as.matrix(res.mds[res.mds$dataset== res.unique$dataset & res.mds$method==res.unique$method & res.mds$i==res.unique$i   & res.mds$run==res.unique$run,c('x','y')]))
  
}

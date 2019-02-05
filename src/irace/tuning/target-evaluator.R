#!/usr/bin/env Rscript --vanilla
options(warn = -1) 

setwd('/Users/Juan/Documents/Projects/u/memoria/app-genetic-mds-2d/src')

suppressMessages(library(ecr))


args <- commandArgs(TRUE)
candidate<-args[1]
instanceId<-args[2]
seed<-args[3]
instanceName<-args[4]
instanceNumber<-args[5]
instances<-tail(args,instanceNumber)
filepaths<-c()
res<-data.frame()
i=1
for (instance in instances){
  id=paste(instance,instanceId,seed,sep='-')
  filepaths[i]=paste0('/tmp/',id)
  
  res<-rbind(cbind(id,readRDS(file=filepaths[i])),res)
  i=i+1
}

maxX1<-max(res$X1)
minX1<-min(res$X1)
maxX2<-max(res$X2)
minX2<-min(res$X2)

res$X1norm<-(res$X1-minX1)/(maxX1-minX1)
res$X2norm<-(res$X2-minX2)/(maxX2-minX2)
tryCatch({
  saveRDS(res,file=paste0('/tmp/',candidate,'-',seed))
}
,error=function(e){
  cat(paste0('/tmp/',candidate,'-',seed))
}
)
cat(-1*computeHV(t(as.matrix(res[res$id==paste(candidate,instanceId,seed,sep='-'),c("X1norm","X2norm")])),c(2,2)))


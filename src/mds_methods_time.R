library(vegan)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
library(usedist)

##Puntos
#cmdscale(M,k=2)
#vegan::wcmdscale(M, k=2, w=rep(1,ncol(M)))
#labdsv::pco(b)$points
#ape::pcoa(M)$vectors
#ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li
#smacofSym(M,ndim=2)$conf

getstress <- function(D,points){
    D1=dist(points)
    raw<-sum((D-D1)^2)
    stress<-raw/sum(D^2)
    mse<-raw/(ncol(as.matrix(D))^2)
    normalized<-sqrt(stress)
    sstress<-sqrt(sum((D^2-D1^2)^2)/sum((D^2)^2))
    return(unname(c(raw,stress,mse,normalized,sstress)))
}



mdsrun <- function(D){

    cmdscaleTime=system.time(cmdscaleDist<-cmdscale(D,k=2))[2:3]
    veganTime= system.time(veganDist<-vegan::wcmdscale(D, k=2))[2:3]
    ecodistTime=system.time(ecodistDist<-ecodist::pco(D)$vectors[1:2])[2:3]
    labdsvTime=system.time(labdsvDist<-labdsv::pco(D)$points)[2:3]
    apeTime=system.time(apeDist<-ape::pcoa(D)$vectors)[2:3]
    ade4Time=system.time(ade4Dist<-ade4::dudi.pco(D, scannf = FALSE, nf = 2)$li)[2:3]
    smacofTime=system.time(smacofDist<-smacof::smacofSym(D,ndim=2)$conf)[2:3]

    cmdscaleStress<-getstress(D,cmdscaleDist)
    veganStress<-getstress(D,veganDist)
    ecodistStress<-getstress(D,ecodistDist)
    labdsvStress<-getstress(D,labdsvDist)
    apeStress<-getstress(D,apeDist)
    ade4Stress<-getstress(D,ade4Dist)
    smacofStress<-getstress(D,smacofDist)

    return (unname(c(cmdscaleTime,cmdscaleStress,veganTime,veganStress,ecodistTime,ecodistTime,labdsvTime,labdsvStress,apeTime,apeStress,ade4Time,ade4Stress,smacofTime,smacofStress)))
}

#crear matriz para prueba de tamaño n
fn='/tmp/data'
if (file.exists(fn)) file.remove(fn)
n<-8000
i<-10
set.seed(1)
D=dist(matrix(runif(n*2),ncol=2))

while(i<n){
    res<-mdsrun(dist_subset(D,1:i))
    line<-unname(c(i,res,'\n'))
    cat(paste(line,collapse='|'),file=fn,append=TRUE)
    print(c(i,date()))
    i<-floor(i*1.2)

}

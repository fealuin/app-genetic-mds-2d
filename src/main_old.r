library(kknn)
library(mco)
library(nsga2R)
library(emoa)
library(ggplot2)


setwd('~/Documents/Projects/u/memoria/app-genetic-mds-2d/src/')
data(glass)
gen=100
source('./population/population.r')
D1=dist(glass[,2:10])
D2=as.dist(cor(t(glass[,2:10])))
M=glass[,2:10]
size=10
pop=Population(D1,D2,size)
initMethod='cmdscaleMean'
pop$initialize(type=initMethod)
pop$setFitness()
data=list()
#puntos=cmdscale(D)
#pop$getIndividual(1)$setData(puntos[-1,])
#pop$getIndividual(1)$setAnchor(puntos[1,])
#pop$setFitness()

data[[1]]<-data.frame(pop$getOrderByFitness())

for( i in 2:gen){
  pop2=pop$getCrossOver()
  pop2$setMutation(radio=0.07)
  pop2$setFitness()
  pop$setIndividuals(append(pop$getIndividuals(),pop2$getIndividuals()))
  pop$orderByFitness()
  pop$setIndividuals(pop$getIndividuals()[1:size])
  print(c(i,pop$getFitness()[1,]))
  data[[i]]<-data.frame(pop$getOrderByFitness())
  #if(pop$getFitness()[1]<18308.55) break
}


x=unlist(lapply(data,function(x) x[,1]))
y=unlist(lapply(data,function(x) x[,2]))
x.mean=mean(x)
x.sd=sd(x)
x.max=max(x)
x.min=min(x)
y.mean=mean(y)
y.sd=sd(y)
y.max=max(y)
y.min=min(y)
data<-lapply(data,function(x) cbind((x[,1]-x.min)/(x.max-x.min),(x[,2]-y.min)/(y.max-y.min),x))

#data<-lapply(data,function(x) cbind((x[,1]-x.mean)/x.sd,(x[,2]-y.mean)/y.sd,x))
#lapply(data,function(x) colnames(x)<-c('objetive1Norm','objetive2Norm','objetive1','objetive2','ndRank','crowdingRank'))
save(data,file='plot/data.RData')
#+ theme_bw()
setwd('~/Documents/Projects/u/memoria/app-genetic-mds-2d/src/plot/images/')
for(i in 1:gen){
  if(TRUE){
    g=(ggplot(data[[i]][,1:2],aes(x=data[[i]][,1],y=data[[i]][,2]))
      +geom_point(aes(colour=as.factor(data[[i]][,5])))
      + theme_bw()
      +geom_line(aes(group=data[[i]][,5],colour=as.factor(data[[i]][,5])))
      +coord_cartesian(xlim = c(0,1),ylim=c(0,1))
      +ggtitle(paste('Generacion:',i,initMethod))
      +xlab(paste('Objetivo1 Norm',round(x.max),round(x.min)))
      +ylab(paste('Objetivo2 Norm',round(y.max),round(y.min)))
      +labs(colour='Nivel')
      + theme(legend.position="none")
    #  +geom_point(data[[i]][which(min(data[[i]])]))
    )
    #png(paste(i,'.png',sep=''))
    png(sprintf('%05d.png',i))
    plot(g)
    dev.off()
  }
}

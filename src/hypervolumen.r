library(RMySQL)
library(imputeTS)#Interpolation
library(ecr)#hypervolumen

library(lattice)
library(latticeExtra)

con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')
rs=dbSendQuery(con,'select id from parameters')
params=fetch(rs,n=-1)

apply(
  params,
  1,
  function(par){
    sql=paste('select xnorm,ynorm from results_final where parameters_id=',par,sep='')
    rs=dbSendQuery(con,sql)
    data=fetch(rs,n=-1)
    hypervolumen=computeHV(t(as.matrix(data)),c(2,2))
    sql=paste('update parameters set hyper_volumen=',hypervolumen,'where id=',par,sep='')
    dbSendQuery(con,sql)
    dbClearResult(rs)
    #print(par,hypervolumen)
  }
)


sql=paste('select * from parameters')
rs=dbSendQuery(con,sql)
data=fetch(rs,n=-1)
dbClearResult(rs)



dbDisconnect(con)

#group by dataset
#missing data
population_size=c(20,30,40,60,70,80,90)
radio=seq(0.1,1,by=0.1)
dataset=c('glass','iris','diabetes')
hyper_volumen=NA
data_extra=expand.grid(population_size,radio,dataset,hyper_volumen)
names(data_extra)=c('population_size','radio','dataset','hyper_volumen')

data_plot=aggregate(hyper_volumen~population_size+radio+dataset,data,mean)


data_new=merge(data_plot,data_extra,all=TRUE)
data_plot=na.interpolation(data_new)

colores=rep("white",times=nrow(data_plot))
colores[which.max(unlist(data_plot['hyper_volumen']))]="red"

a=cloud(
  hyper_volumen~population_size+radio|dataset,
  #z~x+y,

  data=data_plot,
  panel.3d.cloud=panel.3dbars,
  col.facet=colores,
  #cex=0,
  xbase=10, ybase=0.1,
  xlim=c(-10,110), ylim=c(-0.1,1.1),
  scales=list(arrows=FALSE,col=1,distance = 2),
  par.settings = list(axis.line = list(col = 'black')),
  screen = list(z = 30, x = -30, y=0),
  #alpha.facet = 1.00,
  #border = 'red',
  #pretty=true,
  zlab='hv',
  xlab='p',
  ylab='r',
  #par.box=list(col=NA)
  #col.facet='red'
  par.box=list(col=c('black',NA,NA,NA,NA,NA,'black',NA,'black',NA,NA,NA))
  #scpos=list(x=c(1,3),y=2)
  #zoom=1.00
  #drape=TRUE
  #shade=TRUE
)


plot(a)




#no dataset
#missing data
population_size=c(20,30,40,60,70,80,90)
radio=seq(0.1,1,by=0.1)
#dataset=c('glass','iris','diabetes')
hyper_volumen=NA
data_extra=expand.grid(population_size,radio,hyper_volumen)
names(data_extra)=c('population_size','radio','hyper_volumen')

data_plot=aggregate(hyper_volumen~population_size+radio,data,mean)


data_new=merge(data_plot,data_extra,all=TRUE)
data_plot=na.interpolation(data_new)

colores=rep("white",times=nrow(data_plot))
colores[which.max(unlist(data_plot['hyper_volumen']))]="red"

a=cloud(
  hyper_volumen~population_size+radio,
  #z~x+y,

  data=data_plot,
  panel.3d.cloud=panel.3dbars,
  col.facet=colores,
  #cex=0,
  xbase=10, ybase=0.1,
  xlim=c(-10,110), ylim=c(-0.1,1.1),zlim=c(3.3,4),
  scales=list(arrows=FALSE,col=1,distance = 2),
  par.settings = list(axis.line = list(col = 'black')),
  screen = list(z = 30, x = -30, y=0),
  #alpha.facet = 1.00,
  #border = 'red',
  #pretty=true,
  zlab='hv_mean',
  xlab='population',
  ylab='radio',
  #par.box=list(col=NA)
  #col.facet='red'
  par.box=list(col=c('black','black',NA,NA,'black',NA,'black','black','black',NA,NA,NA)),
  #scpos=list(x=c(1,3),y=2)
  zoom=0.6
  #drape=TRUE
  #shade=TRUE
)


plot(a)

#no dataset
#missing data
population_size=c(20,30,40,60,70,80,90)
radio=seq(0.1,1,by=0.1)
#dataset=c('glass','iris','diabetes')
hyper_volumen=NA
data_extra=expand.grid(population_size,radio,hyper_volumen)
names(data_extra)=c('population_size','radio','hyper_volumen')

data_plot=aggregate(hyper_volumen~population_size+radio,data,mean)


data_new=merge(data_plot,data_extra,all=TRUE)
data_plot=na.interpolation(data_new)

colores=rep("white",times=nrow(data_plot))
colores[which.max(unlist(data_plot['hyper_volumen']))]="red"

a=cloud(
  hyper_volumen~population_size+radio,
  #z~x+y,

  data=data_plot,
  panel.3d.cloud=panel.3dbars,
  col.facet=colores,
  #cex=0,
  xbase=10, ybase=0.1,
  xlim=c(-10,110), ylim=c(-0.1,1.1),zlim=c(3.3,4),
  scales=list(arrows=FALSE,col=1,distance = 2),
  par.settings = list(axis.line = list(col = 'black')),
  screen = list(z = 30, x = -30, y=0),
  #alpha.facet = 1.00,
  #border = 'red',
  #pretty=true,
  zlab='hv_mean',
  xlab='population',
  ylab='radio',
  #par.box=list(col=NA)
  #col.facet='red'
  par.box=list(col=c('black','black',NA,NA,'black',NA,'black','black','black',NA,NA,NA)),
  #scpos=list(x=c(1,3),y=2)
  zoom=0.6
  #drape=TRUE
  #shade=TRUE
)


plot(a)

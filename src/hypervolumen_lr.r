library(RMySQL)
library(imputeTS)#Interpolation
library(ecr)#hypervolumen
library(plotly)
library(lattice)
library(latticeExtra)

con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')

id=c(1,2,3)
gens=1:10000
results_lr_ac=expand.grid(id,gens)
names(results_lr_ac)=c('parameters_id','generation')
dbSendQuery(con,'truncate table results_lr_ac')
dbWriteTable(con,'results_lr_ac',results_lr_ac,row.name=FALSE,append=TRUE)

rs=dbSendQuery(con,'select parameters_id,generation from results_lr_ac')
params=fetch(rs,n=-1)

apply(
  params,
  1,
  function(par){
    sql=paste('select xnorm,ynorm from results_lr where parameters_id=',par[1],' and generation=',par[2],sep='')
    rs=dbSendQuery(con,sql)
    data=fetch(rs,n=-1)
    hypervolumen=computeHV(t(as.matrix(data)),c(2,2))
    sql=paste('update results_lr_ac set hyper_volume=',hypervolumen,'where parameters_id=',par[1],' and generation=',par[2],sep='')
    dbSendQuery(con,sql)
    dbClearResult(rs)
    #print(par,hypervolumen)
  }
)


sql=paste('select * from results_lr_ac')
rs=dbSendQuery(con,sql)
data=fetch(rs,n=-1)
dbClearResult(rs)
nround=3
#plot(data$generation,data$hyper_volume)
gen=data[data$parameters_id==1,]$generation
glass=round(data[data$parameters_id==1,]$hyper_volume,nround)
iris=round(data[data$parameters_id==2,]$hyper_volume,nround)
diabetes=round(data[data$parameters_id==3,]$hyper_volume,nround)
#data2=data.frame(gen,iris,glass,diabetes)
data2=data.frame(gen,glass,iris,diabetes)
a <- list(
  #autotick = FALSE,
  #ticks = "outside",
  #tick0 = 1,
  #dtick = 0.25,
  #ticklen = 5,
  #tickwidth = 2,
  #tickcolor = toRGB("blue"),
  range=c(3,5)
)

plot_ly(data2,x=~gen,y=~glass,mode='lines',name='glass',type='scatter')%>%
  add_trace(y = ~iris, name = 'iris', mode = 'lines') %>%
  add_trace(y = ~diabetes, name = 'diabetes', mode = 'lines')%>%
layout(yaxis=a)


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


#data test estadístico
data_csv=paste(paste(data$initialization,data$radio,data$population_size,sep='+'),data$dataset,data$hyper_volumen,sep=',')

write.csv(data_csv,file = "data_mds.csv",row.names=FALSE,col.names=FALSE)

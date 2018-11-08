library(RMySQL)
library(imputeTS)#Interpolation
library(ecr)#hypervolumen
library(plotly)
library(lattice)
library(latticeExtra)

con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')

rs=dbSendQuery(con,'select id from parameters_tests')
id=unlist(fetch(rs,n=-1))
gens=1:160
na=0.0
results_lr_ac=expand.grid(id,gens,na)
names(results_lr_ac)=c('parameters_id','generation','hyper_volume')
dbSendQuery(con,'truncate table results_tests_ac')
dbWriteTable(con,'results_tests_ac',results_lr_ac,row.name=FALSE,overwrite=TRUE)

rs=dbSendQuery(con,'select parameters_id,generation from results_tests_ac')
params=fetch(rs,n=-1)

apply(
  params,
  1,
  function(par){
    sql=paste('select xnorm,ynorm from results_tests where parameters_id=',par[1],' and generation=',par[2],sep='')
    rs=dbSendQuery(con,sql)
    data=fetch(rs,n=-1)
    hypervolumen=computeHV(t(as.matrix(data)),c(2,2))
    sql=paste('update results_tests_ac set hyper_volume=',hypervolumen,'where parameters_id=',par[1],' and generation=',par[2],sep='')
    dbSendQuery(con,sql)
    dbClearResult(rs)
    #print(par,hypervolumen)
  }
)

sql=paste('select * from results_tests_ac')
rs=dbSendQuery(con,sql)
data=fetch(rs,n=-1)
dbClearResult(rs)
nround=3
#plot(data$generation,data$hyper_volume)
gen=data[data$parameters_id==1,]$generation
glass=round(data[data$parameters_id==1,]$hyper_volume,nround)
iris=round(data[data$parameters_id==2,]$hyper_volume,nround)
diabetes=round(data[data$parameters_id==3,]$hyper_volume,nround)
abalone=round(data[data$parameters_id==4,]$hyper_volume,nround)
bcw=round(data[data$parameters_id==5,]$hyper_volume,nround)
ionosphere=round(data[data$parameters_id==6,]$hyper_volume,nround)
fluTrees=round(data[data$parameters_id==141,]$hyper_volume,nround)
#data2=data.frame(gen,iris,glass,diabetes)
data2=data.frame(gen,glass,iris,diabetes,abalone,bcw,ionosphere,fluTrees)
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
  add_trace(y = ~abalone, name = 'abalone', mode = 'lines')%>%
  add_trace(y = ~bcw, name = 'bcw', mode = 'lines')%>%
  add_trace(y = ~ionosphere, name = 'ionosphere', mode = 'lines')%>%
  add_trace(y = ~fluTrees, name = 'fluTrees', mode = 'lines')%>%
layout(yaxis=a)

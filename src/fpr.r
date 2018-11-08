#Non Dominated Pareto Front Covertura frontera de pareto
library(nsga2R)
library(RMySQL)
library(zoo)

# sql=paste('select xnorm,ynorm from results_lr where parameters_id=1')
# rs=dbSendQuery(con,sql)
# data=fetch(rs,n=-1)
# 
# a=fastNonDominatedSorting(data)
con <- dbConnect(MySQL(), user='root', password='demos30', dbname='memoria', host='localhost')
sql=paste('select generation,hyper_volume from results_lr_ac where parameters_id=3')
rs=dbSendQuery(con,sql)
data=fetch(rs,n=-1)

#lag(data$hyper_volume,-10)
data$hyper_volume[50]
  #rollapply(data$hyper_volume,2,mean)
for (i in  101:length(data$hyper_volume)){
  
  hv<-round(data$hyper_volume[((i-100):i)],10)
  #print(paste(data$generation[i], sd(hv)/mean(hv),sep="|"))
  if(sd(hv)/mean(hv)<0.001){
    print(paste(data$generation[i], sd(hv)/mean(hv),sep="|"))
    break
  }
  #print(win)
  #data$hyper_volume[i]
  #sd(c(1.1,1,1,1,1))/mean(c(1.1,1,1,1,1))
}

#corte 160 generaciones

sql=paste('select xnorm,ynorm from results_lr where parameters_id=1 and generation<=160')
rs=dbSendQuery(con,sql)
data=fetch(rs,n=-1)
 
a=fastNonDominatedSorting(data)




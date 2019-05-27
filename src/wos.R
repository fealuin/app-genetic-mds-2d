library(treemap)
library(plotly)
scopus_mds<-read.csv('../datasets/wos_mds.txt',sep = '\t',header = TRUE,stringsAsFactors = FALSE)
#wos_mds[-100,]
#treemap(wos_mds,index='area',vSize='records',type='index',palette = terrain.colors(24) )
names(scopus_mds)<-c('areaen','area','count')

scopus_mds<-read.csv('../datasets/scopus_mds.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

scopus_mds[26:nrow(scopus_mds),]$area<-'Otros'

scopus_mds$count<-as.numeric(scopus_mds$count)
scopus_mds$count<-100*scopus_mds$count/sum(scopus_mds$count)

scopus_mds<-aggregate(scopus_mds$count,by=list(scopus_mds$area),FUN=sum)
names(scopus_mds)=c('area','count')
scopus_mds<-scopus_mds[order(scopus_mds$count,decreasing = TRUE),]



scopus_mds$area<-factor(scopus_mds$area, levels = c(as.character(scopus_mds$area)))



#plot_ly(scopus_mds,values=~count,type = 'pie',labels=~area)

p<-plot_ly(scopus_mds,x=~area,y=~count,type = 'bar',text=scopus_mds$count) %>%
  layout(title = "Documentos por área del conocimiento",
         xaxis = list(title = "Área del conocimiento"),
         yaxis = list(title = "Porcentaje de documentos"))
p


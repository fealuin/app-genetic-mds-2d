library(treemap)
library(plotly)
scopus_mds<-read.csv('../datasets/wos_mds.txt',sep = '\t',header = TRUE,stringsAsFactors = FALSE)
#wos_mds[-100,]
#treemap(wos_mds,index='area',vSize='records',type='index',palette = terrain.colors(24) )
names(scopus_mds)<-c('areaen','area','count')

scopus_mds<-read.csv('../datasets/scopus_mds.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

scopus_mds[20:nrow(scopus_mds),]$area<-'Otros'

scopus_mds$count<-as.numeric(scopus_mds$count)
scopus_mds$count<-100*scopus_mds$count/sum(scopus_mds$count)

scopus_mds<-aggregate(scopus_mds$count,by=list(scopus_mds$area),FUN=sum)
names(scopus_mds)=c('area','count')
scopus_mds<-scopus_mds[order(scopus_mds$count,decreasing = TRUE),]



scopus_mds$area<-factor(scopus_mds$area, levels = c(as.character(scopus_mds$area)))



#plot_ly(scopus_mds,values=~count,type = 'pie',labels=~area)

p<-plot_ly(scopus_mds,x=~count,y=~area,type = 'bar',text=scopus_mds$count,orientation='h')%>%

  layout(
         xaxis = list(title = "",showgrid = FALSE, zeroline = FALSE, showticklabels = T),
         yaxis = list(title = "",showgrid = FALSE, zeroline = FALSE, showticklabels = T),
         legend = list(font = list(size = 22)),
         font = list(size = 22)
         )

p

p<-plot_ly(type='bar')%>%
  layout(title = NULL,
         xaxis = list(title = '', type = 'categorical', nticks = 10, categoryorder = 'trace'),
         yaxis = list(title = ""),
         showlegend = TRUE)

apply(scopus_mds,1,function(x) p<-add_trace(p,x=x[['area']],y=x[['count']]))

p

for(i in 1:nrow(scopus_mds)){
  p<-add_trace(p,x=scopus_mds[i,]$area,y=scopus_mds[i,]$count,mode='bar')
}





p<-plot_ly(scopus_mds,x=sprintf('%02d',1:nrow(scopus_mds)),y=~count,text=scopus_mds$count,textposition='auto',type = 'bar',color=paste(sprintf('%02d',1:nrow(scopus_mds)),scopus_mds$area))%>%
  layout(
    xaxis = list(title = '',showgrid = T, zeroline = T, showticklabels = T),
    yaxis = list(title = '',showgrid = T, zeroline = T, showticklabels = T),
    legend=list(font = list(size = 20)),
    font = list(size = 20)
  )

p


subplot(p,shareX = T)


# p<-plot_ly(scopus_mds,x=~area,y=~count,type = 'bar',showlegend=T) %>%
#   layout(
#          xaxis = list(title = "Área del conocimiento",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(title = "Porcentaje de documentos",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          legend = list(font = list(size = 22)),
#          font = list(size = 22)
#          )
# p

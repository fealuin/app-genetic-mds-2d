source('main.r')
library(plotly)
library(amap)

h2017<-read.csv('../datasets/hospitales/matriz2018.txt',header = TRUE,sep=';')


dh2017<-dist(h2017)
#ch2017<-100*as.dist((1-cor(t(h2017)))/2)
ch2017<-Dist(h2017,method="correlation")
resultado<-geneticMds2(dh2017,ch2017,160,100,radio=0.5)


desc<-read.csv('../datasets/hospitales/IM_mod.csv',header=TRUE,sep=',')


plot_ly(type='scatter',x=resultado[[1]][resultado[[1]]['X5']==160,][,c(1)],y=resultado[[1]][resultado[[1]]['X5']==160,][,c(2)],text=seq(100))

plot_ly(type='scatter',x=resultado[[1]][,c(1)],y=resultado[[1]][,c(2)])


dist<-resultado[[2]][resultado[[2]]['i']==4,][,c(3)]
cor<-resultado[[2]][resultado[[2]]['i']==4,][,c(4)]


data_plot=data.frame(dist,cor,desc)

p1<-plot_ly(type='scatter',mode='markers')%>%
add_trace(data = data_plot,x=~dist,y=~cor,color=~Complejidad,text=~Nombre,hoverinfo='')
dist<-resultado[[2]][resultado[[2]]['i']==5,][,c(3)]
cor<-resultado[[2]][resultado[[2]]['i']==5,][,c(4)]
data_plot=data.frame(dist,cor,desc)
p2<-     plot_ly(type='scatter',mode='markers' )%>%
add_trace(data = data_plot,x=~dist,y=~cor,color=~Complejidad,text=~Nombre,hoverinfo='')
dist<-resultado[[2]][resultado[[2]]['i']==2,][,c(3)]
cor<-resultado[[2]][resultado[[2]]['i']==2,][,c(4)]
data_plot=data.frame(dist,cor,desc)
p3<-plot_ly(type='scatter',mode='markers')%>%
add_trace(data = data_plot,x=~dist,y=~cor,color=~Complejidad,text=~Nombre,hoverinfo='')
subplot(p1,p2,p3)#,shareY = TRUE)


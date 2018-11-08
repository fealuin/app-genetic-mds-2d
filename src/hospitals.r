h2017<-read.csv('../datasets/hospitales/matriz2018.txt',header = TRUE,sep=';')


dh2017<-dist(h2017)
ch2017<-as.dist(cor(t(h2017)))

resultado<-geneticMds2(dh2017,ch2017,160,100,radio=0.5)


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


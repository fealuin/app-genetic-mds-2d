plotMds<-function(data){
  data<-data.frame(data)

  g<-(ggplot(data[,1:2],aes(x=data[,1],y=data[,2]))
    +geom_point(aes(colour=as.factor(data[,3])))
    +theme_bw()
    +geom_line(aes(group=data[,3],colour=as.factor(data[,3])))
    #+coord_cartesian(xlim = c(0,1),ylim=c(0,1))
    +ggtitle(paste('Generacion:',1))+xlab('Objetivo1 Norm')
    +ylab('Objetivo2 Norm')
    +labs(colour='Nivel')+ theme(legend.position="none")
  )
  plot(g)
}

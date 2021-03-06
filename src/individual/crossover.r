#debe seleccionar el mejor de los hijos según el fitness
crossOver<-function(D,I1,I2,ratio,type="random"){
  n=I1$getNrow()
  m=I1$getNcol()
  dataI1=I1$getData()
  dataI2=I2$getData()
  if(type=="random"){

    s=sample(n,size=ratio*n)
    child1=Individual(n,m)
    child2=Individual(n,m)

    child1Data<-dataI1
    child2Data<-dataI2

    #mod
    #center=sample(n,size=1)
    #child1Data<-sweep(dataI1,2,dataI1[center,],'-')+1
    #child2Data<-sweep(dataI2,2,dataI2[center,],'-')+1

    child1Data[s,]<-dataI2[s,]
    child2Data[s,]<-dataI1[s,]
    child1$setData(child1Data)
    child2$setData(child2Data)
    child1$setFitness(D)
    child2$setFitness(D)
    if(sum(child1$getFitness())<sum(child2$getFitness())){
      return(child1)
    }
    else{
      return(child2)
    }
  }
  else{
    sprintf("Type %s don't exist",type)
    return(FALSE)
  }
}

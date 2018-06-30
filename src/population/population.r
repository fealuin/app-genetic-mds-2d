source('individual/individual.r')
#crossover primero, segundo mutación

Population<-function(distance,size=100,m=2){
  #distance matrix
  D<-distance
  n<-nrow(as.matrix(D))
  thisEnv <- environment()
  individuals<-list()

  me<-list(
    thisEnv = thisEnv,
    getEnv = function(){
      return(get("thisEnv",thisEnv))
    },
    getDistance=function() {
      return(get("D",thisEnv))
    },
    getIndividuals=function(){
      return(get("individuals",thisEnv))
    },
    setIndividuals=function(I){
      return(assign("individuals",I,thisEnv))
    },
    getIndividual=function(i){
      return(me$getIndividuals()[[i]])
    },
    initialize=function(type="radial",max=100){
      init=list()
      for (i in 1:size){
        init[[i]]=Individual(n,m)
      }
      me$setIndividuals(init)
      lapply(me$getIndividuals(),function(x) x$initialize(D,type,max))
      return("ok")
    },
    setFitness=function(){
      lapply(me$getIndividuals(),function(x) x$setFitness(D))
      return("ok")
    },
    getFitness=function() {
      return(unlist(lapply(me$getIndividuals(),function(x) x$getFitness())))
    },
    orderByFitness=function(decreasing=FALSE){
      return(me$setIndividuals(me$getIndividuals()[order(me$getFitness(),decreasing=decreasing)]))
    },
    setMutation=function(p=0.4,type="radio") {
      lapply(me$getIndividuals(),function(x) if(runif(1)<p){x$setMutation()})
      return("ok")
    },
    getCrossOver=function(ratio=0.4,type="random"){
      M<-Population(D,size,m)
      init<-list()
      for (i in 1:size){
        parent1<-me$getIndividual(me$getKTournamentWinner())
        parent2<-me$getIndividual(me$getKTournamentWinner())
        init[[i]]<-crossOver(D,parent1,parent2,ratio)
      }
      M$setIndividuals(init)
      return(M)
    },
    getKTournamentWinner=function(k=4){
      fit=me$getFitness()
      invididualsSample=sample(1:size,k)
      winner=which.min(fit[invididualsSample])
      return(invididualsSample[winner])
    }
  )
  class(me) <- append(class(me),"Population")
  return(me)
}

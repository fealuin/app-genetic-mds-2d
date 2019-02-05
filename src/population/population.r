source('individual/individual.r')
#crossover primero, segundo mutación

Population<-function(D1,D2,size=100,m=2){
  #distance matrix
  D<-list(D1,D2)
  n<-nrow(as.matrix(D1))
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
    getData=function(){
      i=1
      d=matrix(ncol=m+2,nrow=0)
      for(individual in me$getIndividuals()){
        data=individual$getData()
        d=rbind(d,cbind(i,seq.int(nrow(data)),data))
        i=i+1
      }
      #names(d)<-c('individual_id','point_id','x','y')
      return(data.frame(d))
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
    initialize=function(type="pca",max=100){
      init=list()
      for (i in 1:size){
        init[[i]]=Individual(n,m)
      }
      me$setIndividuals(init)
      lapply(me$getIndividuals(),function(x) x$initialize(me$getDistance(),type,max))
      return("ok")
    },
    setFitness=function(){
      lapply(me$getIndividuals(),function(x) x$setFitness(me$getDistance()))
      return("ok")
    },
    getFitness=function() {
      M=matrix(unlist(lapply(me$getIndividuals(),function(x) x$getFitness())),ncol=2,byrow='true')
      return(M)
    },
    orderByFitness=function(decreasing=FALSE){
      return(me$setIndividuals(me$getIndividuals()[order(me$getParetoRanking(),-me$getCrowdingDistance(),decreasing=decreasing)]))
    },
    getOrderByFitness=function(decreasing=FALSE){
      return(cbind(me$getFitness(),me$getParetoRanking(),me$getCrowdingDistance())[order(me$getParetoRanking(),-me$getCrowdingDistance(),decreasing=decreasing),])
    },
    getParetoRanking=function(){
      sort=rep(0,size)
      rank=fastNonDominatedSorting(me$getFitness())
      for(i in 1:length(rank)){
        sort[rank[[i]]]=i
      }
      return(sort)
    },
    getCrowdingDistance=function() {

      #Crowding distance debiera ser por ranking
      #cd=crowding_distance(t(cbind(me$getFitness(),me$getParetoRanking())))
      ##old
        #cd=crowding_distance(t(me$getFitness()))
        #return(cd)
        res=c()

        fit=me$getFitness()
        paretoRanking=me$getParetoRanking()
        for(rank in unique(paretoRanking)){
          ind<-which(paretoRanking==rank)
          res[ind]<-crowding_distance(t(fit[ind,]))
        }
        return(res)
    },
    setMutation=function(p=runif(1),type="radio",radio=0) {
      lapply(me$getIndividuals(),function(x) if(runif(1)<p){x$setMutation(type=type,radio=radio)})
      return("ok")
    },
    getCrossOver=function(ratio=runif(1),type="random"){
      M<-Population(D[[1]],D[[2]],size,m)
      init<-list()
      for (i in 1:size){
        parent1<-me$getIndividual(me$getKTournamentWinner())
        parent2<-me$getIndividual(me$getKTournamentWinner())
        init[[i]]<-crossOver(me$getDistance(),parent1,parent2,ratio)
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

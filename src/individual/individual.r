  #Individual class
  source('individual/fitness.r')
  source('individual/mutation.r')
  source('individual/crossover.r')
  source('individual/initialization.r')

  Individual<-function(nr=10,m=2) {
    thisEnv <- environment()
    anchor<-0
    data<-matrix(nrow=nr,ncol=m)
    n<-nr
    m<-m
    fitness=c(0,0)
    me<-list(
      thisEnv = thisEnv,
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },
      getNrow = function(){
        return(get("n",thisEnv))
      },
      getNcol = function(){
        return(get("m",thisEnv))
      },
      getAnchor = function(){
        return(get("anchor",thisEnv))
      },
      setAnchor= function(a){
        return(assign("anchor",a,thisEnv))
      },
      #Data
      getDataAnchor=function() {
        return(rbind(me$getAnchor(),me$getData()))
      },
      getData = function(){
        return(get("data",thisEnv))
      },
      setData=function(M){
        return (assign("data",M,thisEnv))
      },
      #Initialization
      initialize=function(D,type='pca',max=1){
        return (assign("data",initialize(me,D,type,max),thisEnv))
      },
      #Fitness
      getFitness=function(){
        return(get("fitness",thisEnv))
      },
      setFitness=function(D,type='rawStress'){
        return(assign("fitness",fitness(D,me$getData(),type),thisEnv))
      },
      setFitnessValue=function(f){
        return(assign("fitness",f,thisEnv))
      },

      #Mutation
      setMutation=function(type='radio',radio=0){
        return(mutate(I=me,radio=radio,p=0.07,type=type))
      },
      getCrossOver=function(D,I,ratio=0.2,type="random"){
        return(crossOver(D,me,I,ratio,type))
      }

    )
    class(me) <- append(class(me),"Individual")
    return(me)
  }

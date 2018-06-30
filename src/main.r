library(kknn)

data(glass)

source('./population/population.r')
D=dist(glass[,2:10])
size=100
pop=Population(D,size)
pop$initialize()
pop$setFitness()

#puntos=cmdscale(D)
#pop$getIndividual(1)$setData(puntos[-1,])
#pop$getIndividual(1)$setAnchor(puntos[1,])
#pop$setFitness()


for( i in 1:500){
  pop2=pop$getCrossOver()
  pop2$setMutation()
  pop2$setFitness()
  pop$setIndividuals(append(pop$getIndividuals(),pop2$getIndividuals()))
  pop$orderByFitness()
  pop$setIndividuals(pop$getIndividuals()[1:size])
  print(c(i,pop$getFitness()[1]))
  if(pop$getFitness()[1]<18308.55) break
}

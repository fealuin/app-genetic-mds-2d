fnc_create_pop=function(algorithms,max_gen)
{
#Built rulette evaluation
pop_tmp=length(algorithms$fit)
roulette=round(c(0,cumsum(as.numeric(algorithms$fitmean)/sum(as.numeric(algorithms$fitmean)))[1:(pop_tmp)]),2)

for (a in 1:pop_tmp)
 {n_var=5 #Number of variables to interchange
  selection=findInterval(runif(n_var,0,1.5),roulette)

  #Generations
  if (selection[1]>pop_tmp) {input$gen[a]=sample(max_gen,1,replace=T)}
  if (selection[1]<=pop_tmp){input$gen[a]=algorithms$gen[selection[1]]}
  
  #Pop size
  if (selection[2]>pop_tmp) {input$pop_size[a]=sample(10:100,1)}
  if (selection[2]<=pop_tmp){input$pop_size[a]=algorithms$pop_size[selection[2]]}
  
  #Cross rate
  if (selection[3]>pop_tmp) {input$cross_rate[a]=round(runif(1,0.1,0.4),2)}
  if (selection[3]<=pop_tmp){input$cross_rate[a]=algorithms$cross_rate[selection[3]]}
  
  #Mute rate
  if (selection[4]>pop_tmp) {input$mut_rate[a]=round(runif(1,0.1,0.4),2)}
  if (selection[4]<=pop_tmp){input$mut_rate[a]=algorithms$mut_rate[selection[4]]}
  
  #Ini methods
  if (selection[5]>pop_tmp) {input$met_ini[a]=sample(c("RAND","GPS","KNN","OWNN","PAM","SNN","RF"),1,replace=T)}
  if (selection[5]<=pop_tmp){input$met_ini[a]=algorithms$met_ini[selection[5]]}

  }

  return(input)
}



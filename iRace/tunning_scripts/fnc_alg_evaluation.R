fnc_alg_evaluation=function(main,instructions,input,n_algorithms)
{
  output=NULL
  for (n in 1:n_algorithms)
  {
    #Save the configuration
    instructions$application$ending=input$gen[[n]]
    instructions$population$size=input$pop_size[[n]]
    instructions$population$initialization$method=input$met_ini[[n]]
    instructions$genetic_operations$crossover$rate=input$cross_rate[[n]]
    instructions$genetic_operations$mutation$rate=input$mut_rate[[n]]
    write_yaml(instructions,"instructions.yml")
    
    #Get the evaluations
    perf_tmp=NULL
    out_tmp=NULL
    for (a in 1:n_eval)
    { out_tmp=main()
      #invisible(capture.output(out_tmp = main()))
      perf_tmp=c(perf_tmp,out_tmp$fitness[[1]])
    }
    
    #Save the ouput
    output$fit[[n]]=perf_tmp
    output$gen[[n]]=input$gen[[n]]
    output$pop_size[[n]]=input$pop_size[[n]]
    output$met_ini[[n]]=input$met_ini[[n]]
    output$cross_rate[[n]]=input$cross_rate[[n]]
    output$mut_rate[[n]]=input$mut_rate[[n]]
  }
  
  #Sorting the output by mean
  output$fitmean=NULL
  for (a in 1:n_algorithms)
  {output$fitmean[[a]]=mean(output$fit[[a]])}
  indexes=order(output$fitmean,decreasing = T)
  output$gen=output$gen[indexes]
  output$pop_size=output$pop_size[indexes]
  output$met_ini=output$met_ini[indexes]
  output$cross_rate=output$cross_rate[indexes]
  output$mut_rate=output$mut_rate[indexes]
  output$fitmean=output$fitmean[indexes]
  output$fit=output$fit[indexes]
  
  return(output)
} 
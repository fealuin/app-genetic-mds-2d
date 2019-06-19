library(plan)
g<-new('gantt')
g<-ganttAddTask(g,"hola")
g<-ganttAddTask(g,"Physical Oceanography (A)", "2016-09-03", "2016-12-05", done=100)


plot(g)


m<-mermaid("gantt
        
        dateFormat  YYYY-MM-DD
     
        EM1: EM1, 2019-03-15,3w
        
        EM2: EM2,after EM1,3w
        
        EM3: EM3, after EM2,6w
        
        EM4: EM4, after EM3,2w
        
        EM5: EM5, after EM4,2w",Scale='%d'
)
m$x$config=list(ganttConfig = list(
  
  # Make sure the axis labels are formatted correctly
  axisFormatter = list(list(
    "%W", # New data format
    htmlwidgets::JS('function(d){ return d}') # Select dates to format
  ))))
m

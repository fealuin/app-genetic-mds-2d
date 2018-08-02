mutate<-function(I,p=0.4,radio=0.08,type='radio'){
  n=I$getNrow()
  m=I$getNcol()
  if(type=='flipPoints'){
    I$setData(2*I$getData()[sample(n,size=p*n),])
  }
  if(type=='radio'){
    data=I$getData()
    for(i in sample(n,p*n)){
      data[i,]=data[i,]*runif(n=2,min=1-radio/2,max=1+radio/2)
    }
    I$setData(data)
  }
}
